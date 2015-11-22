package com.aylasoftware.bendaroni

import shapeless._
import shapeless.syntax.std.tuple._
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.Matrices

case class TriangulatedShape(
    vertices: IndexedSeq[Point],
    triangles: IndexedSeq[(Int, Int, Int)]) {

  private[this] case class Edge(i: Int, j: Int) {
    require(i < j)
  }

  private[this] case class Triangle(i: Int, j: Int, k: Int)

  private[this] case class EdgeNeighborDoodad(
    i: Int,
    j: Int,
    neighborInfo: LeftRightNeighborInfo,
    GTGinvGT: DenseMatrix)

  /*
   * Calculate the top of matrices A1 and A2.  Also calculate
   * matrix G products for each edge.
   */
  def register(): Unit = {
    /*
     * For each edge, we have a data structure
     */
    val edgeToAdjacentTris = triangles.foldLeft(Map.empty[Edge, Seq[Triangle]]) {
      case (map, (p, q, r)) =>
        Seq(p, q, r).combinations(2).foldLeft(map) {
          case (map, Seq(i, j)) =>
            val e = Edge(i min j, i max j)
            map.updated(e, map.getOrElse(e, Seq.empty) :+ Triangle(p, q, r))
        }
    }

    // For each of these edges, calculate G.
    val edgeNeighborDoodads = edgeToAdjacentTris.map {
      case (e, ts) =>
        val vi = vertices(e.i)
        val vj = vertices(e.j)

        val edgeNeighborInfo = getLeftAndRightEdgeNeighbors(e, ts)

        val gRows = edgeNeighborInfo match {
          case _: LeftAndRightNeighbors => 8
          case _                        => 6
        }

        val G = new DenseMatrix(gRows, 2)
        G.set(0, 0, vi.x); G.set(0, 1, vi.y)
        G.set(1, 0, vi.y); G.set(1, 1, -vi.x)
        G.set(2, 0, vj.x); G.set(2, 1, vj.y)
        G.set(3, 0, vj.y); G.set(3, 1, -vj.x)

        edgeNeighborInfo match {
          case LeftAndRightNeighbors(l, r) =>
            val vl = vertices(l)
            val vr = vertices(r)
            G.set(4, 0, vl.x); G.set(4, 1, vl.y)
            G.set(5, 0, vl.y); G.set(5, 1, -vl.x)
            G.set(6, 0, vr.x); G.set(6, 1, vr.y)
            G.set(7, 0, vr.y); G.set(7, 1, -vr.x)
          case LeftNeighbor(l) =>
            val vl = vertices(l)
            G.set(4, 0, vl.x); G.set(4, 1, vl.y)
            G.set(5, 0, vl.y); G.set(5, 1, -vl.x)
          case RightNeighbor(r) =>
            val vr = vertices(r)
            G.set(4, 0, vr.x); G.set(4, 1, vr.y)
            G.set(5, 0, vr.y); G.set(5, 1, -vr.x)
        }

        val GTG = new DenseMatrix(2, 2)
        G.transAmult(G, GTG)

        val GTGinv = {
          val I = Matrices.identity(2);
          GTG.solve(I, I.copy());
        }

        // (2 x 2) x (2 x gRows) = 2 x gRows
        val GTGinvGT = new DenseMatrix(2, gRows)
        GTGinv.transBmult(G, GTGinvGT)

        EdgeNeighborDoodad(e.i, e.j, edgeNeighborInfo, GTGinvGT)
    }
    
    // TODO:  Finish this up
  }

  //  private[this] case class LeftRightResult(leftNeighbor: Int, rightNeighbor: Int])

  sealed trait LeftRightNeighborInfo
  case class LeftAndRightNeighbors(l: Int, r: Int) extends LeftRightNeighborInfo
  case class LeftNeighbor(l: Int) extends LeftRightNeighborInfo
  case class RightNeighbor(r: Int) extends LeftRightNeighborInfo

  private[this] def getLeftAndRightEdgeNeighbors(e: Edge, adjacentTris: Seq[Triangle]): LeftRightNeighborInfo = {

    val ij = adjacentTris
      .flatMap(_.toList)
      .filterNot(x => x == e.i || x == e.j)

    val (i, jOpt) = ij match {
      case Seq(i)    => (i, None)
      case Seq(i, j) => (i, Some(j))
    }

    val v = vertices(e.i)
    val p = vertices(i)

    // Have to determine if p is on the left or right
    if (v.determinant(p) > 0)
      // p is on the left
      //LeftRightResult(Some(i), jOpt)
      jOpt match {
        case Some(j) => LeftAndRightNeighbors(i, j)
        case None    => LeftNeighbor(i)
      }

    else
      // p is on the right
      jOpt match {
        case Some(j) => LeftAndRightNeighbors(j, i)
        case None    => RightNeighbor(i)
      }
  }

  // TODO:  This should return something.
  def compile(): Unit = {
    ???
  }
}