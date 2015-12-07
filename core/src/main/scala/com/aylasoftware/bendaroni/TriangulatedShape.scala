package com.aylasoftware.bendaroni

import shapeless._
import shapeless.syntax.std.tuple._
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.Matrices
import scala.collection.JavaConverters._

import no.uib.cipr.matrix.DenseVector

import org.aylasoftware.csparse.doubles.Dcs_compress
import org.aylasoftware.csparse.doubles.Dcs_common.Dcs
import org.aylasoftware.csparse.doubles.Dcs_util
import org.aylasoftware.csparse.doubles.Dcs_entry
import org.aylasoftware.csparse.doubles.Dcs_qrsol

// TODO: Use sparse matrices everywhere
case class TriangulatedShape(vertices: IndexedSeq[Point], triangles: IndexedSeq[Triangle]) {

  val w = 1000d

  sealed abstract class NeighborInfo(val indices: Int*) {
    def points: Iterable[Point] = indices.map(vertices)
  }
  case class LeftAndRightNeighbors(l: Int, r: Int) extends NeighborInfo(l, r)
  case class LeftNeighbor(l: Int) extends NeighborInfo(l)
  case class RightNeighbor(r: Int) extends NeighborInfo(r)

  case class EdgeNeighborDoodad(e: Edge, neighborInfo: NeighborInfo, GTGinvGT: DenseMatrix)

  case class CompilationResult(A1: Dcs, A2: Dcs, edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad])
  case class RegistrationResult(L1: DenseMatrix, L2: DenseMatrix, edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad])

  implicit class MultimapOps[K, V](self: Map[K, Seq[V]]) {
    def multiUpdated(key: K, value: V): Map[K, Seq[V]] = self.updated(key, self.getOrElse(key, Seq.empty) :+ value)
  }

  /*
   * Calculate the top of matrices A1 and A2.  Also calculate
   * matrix G products (i.e., GTGinvGT) for each edge.
   */
  def register(): RegistrationResult = {
    val edgeToAdjacentTris = triangles.foldLeft(Map.empty[Edge, Seq[Triangle]]) {
      case (map, t @ Triangle(p, q, r)) =>
        Seq(Edge(p, q), Edge(p, r), Edge(q, r)).foldLeft(map)(_.multiUpdated(_, t))
    }

    val edgeNeighborDoodads = edgeToAdjacentTris.map {
      case (e, ts) =>
        val neighborInfo = getLeftAndRightEdgeNeighbors(e, ts)

        val vi = vertices(e.i)
        val vj = vertices(e.j)

        val pts = Seq(vi, vj) ++ neighborInfo.points

        val G = new DenseMatrix(2 * pts.size, 4)
        pts.iterator.zipWithIndex.foreach {
          case (p, i) =>
            val row = 2 * i
            G.set(row, 0, p.x); G.set(row, 1, p.y); G.set(row, 2, 1);
            G.set(row + 1, 0, p.y); G.set(row + 1, 1, -p.x); G.set(row + 1, 3, 1);
        }

        val GTG = G.transAmult(G, new DenseMatrix(G.numColumns, G.numColumns))

        val GTGinv = {
          val I = Matrices.identity(G.numColumns);
          GTG.solve(I, I.copy());
        }

        val GTGinvGT = GTGinv.transBmult(G, new DenseMatrix(G.numColumns, G.numRows))

        val GTGinvGT_top = new DenseMatrix(2, G.numRows)
        (0 until GTGinvGT.numColumns).foreach { c =>
          GTGinvGT_top.set(0, c, GTGinvGT.get(0, c))
          GTGinvGT_top.set(1, c, GTGinvGT.get(1, c))
        }

        EdgeNeighborDoodad(e, neighborInfo, GTGinvGT_top)
    }.toIndexedSeq

    val L1 = buildMatrixL1(edgeNeighborDoodads)
    val L2 = buildMatrixL2(edgeNeighborDoodads)

    RegistrationResult(L1, L2, edgeNeighborDoodads)
  }

  private[this] def getLeftAndRightEdgeNeighbors(e: Edge, adjacentTris: Seq[Triangle]): NeighborInfo = {
    val ij = adjacentTris
      .flatMap(_.toList)
      .filterNot(x => x == e.i || x == e.j)

    val (i, jOpt) = ij match {
      case Seq(i)    => (i, None)
      case Seq(i, j) => (i, Some(j))
    }

    val vi = vertices(e.i)
    val vj = vertices(e.j)

    val p = vertices(i)

    val vec_vi_vj = vj - vi
    val vec_vi_p = p - vi

    // Have to determine if p is on the left or right
    if (vec_vi_vj.determinant(vec_vi_p) > 0)
      // p is on the left
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

  /*
   * L1 is the top part of matrix A1 that is used in the translation/rotation
   * calculations.
   */
  private[this] def buildMatrixL1(edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad]): DenseMatrix = {
    val L1 = new DenseMatrix(2 * edgeNeighborDoodads.size, 2 * vertices.size)

    // TODO:  Rename these matrices.
    val X8 = new DenseMatrix(2, 8)
    X8.set(0, 0, -1); X8.set(0, 2, 1)
    X8.set(1, 1, -1); X8.set(1, 3, 1)

    val X6 = new DenseMatrix(2, 6)
    X6.set(0, 0, -1); X6.set(0, 2, 1)
    X6.set(1, 1, -1); X6.set(1, 3, 1)

    val E = new DenseMatrix(2, 2)
    val E8 = new DenseMatrix(2, 8)
    val E6 = new DenseMatrix(2, 6)
    edgeNeighborDoodads.iterator.zipWithIndex.foreach {
      case (EdgeNeighborDoodad(Edge(i, j), neighborInfo, matGTGinvGT), edgeIdx) =>
        val vi = vertices(i)
        val vj = vertices(j)
        val eVec = vj - vi

        E.set(0, 0, eVec.x); E.set(0, 1, eVec.y)
        E.set(1, 0, eVec.y); E.set(1, 1, -eVec.x)

        val (outE, outX) = neighborInfo match {
          case _: LeftAndRightNeighbors => (E8, X8.copy)
          case _ => (E6, X6.copy)
        }
        
        E.mult(matGTGinvGT, outE)
        outX.add(-1, outE)
        
        // Unpack the coefficients and stuff them into the matrix L1
        val row1 = 2 * edgeIdx
        val row2 = row1 + 1
        
        (Iterable(i, j) ++ neighborInfo.indices).iterator.zipWithIndex.foreach{case (i, j) =>
          L1.set(row1, 2 * i, outX.get(0, 2 * j))
          L1.set(row1, 2 * i + 1, outX.get(0, 2*j + 1))
          
          L1.set(row2, 2 * i, outX.get(1, 2 * j))
          L1.set(row2, 2 * i + 1, outX.get(1, 2 * j + 1))
        }
    }
    L1
  }

  /*
   * L2 is the top portion of the matrix used for optimizing scale. 
   */
  private[this] def buildMatrixL2(edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad]): DenseMatrix = {
    val L2 = new DenseMatrix(edgeNeighborDoodads.size, vertices.size)

    edgeNeighborDoodads.iterator.zipWithIndex.foreach {
      case (EdgeNeighborDoodad(Edge(i, j), _, _), edgeIdx) =>
        L2.set(edgeIdx, i, -1)
        L2.set(edgeIdx, j, 1)
    }
    L2
  }

  def compile(handleIDs: IndexedSeq[Int], registrationResult: RegistrationResult): CompilationResult = {
    val L1 = registrationResult.L1
    val L2 = registrationResult.L2

    val C1 = buildMatrixC1(handleIDs, w)
    val C2 = buildMatrixC2(handleIDs, w)

    // Copy L1 and C1 into A1.
    val A1 = Dcs_util.cs_spalloc(0, 0, 1, true, true)
    L1.iterator.asScala
      // TODO:  When L1 is sparse, get rid of these checks
      .filter(_.get != 0d)
      .foreach(e => assert(Dcs_entry.cs_entry(A1, e.row, e.column, e.get) != null))

    C1.iterator.asScala
      .filter(_.get != 0d)
      .foreach(e => assert(Dcs_entry.cs_entry(A1, e.row + L1.numRows, e.column, e.get) != null))

    val A1_compressed = Dcs_compress.cs_compress(A1)

    // Copy L2 and C2 into A2.
    val A2 = Dcs_util.cs_spalloc(0, 0, 1, true, true)
    L2.iterator.asScala
      // TODO:  When L2 is sparse, get rid of these checks
      .filter(_.get != 0d)
      .foreach(e => assert(Dcs_entry.cs_entry(A2, e.row, e.column, e.get) != null))

    C2.iterator.asScala
      .filter(_.get != 0d)
      .foreach(e => assert(Dcs_entry.cs_entry(A2, e.row + L2.numRows, e.column, e.get) != null))

    val A2_compressed = Dcs_compress.cs_compress(A2)

    CompilationResult(A1_compressed, A2_compressed, registrationResult.edgeNeighborDoodads)
  }

  def calculateNewVertexPositions(handleIndices: IndexedSeq[Int], handlePositions: IndexedSeq[Point], compilationResult: CompilationResult): IndexedSeq[Point] = {
    val b1 = new Array[Double](2 * compilationResult.edgeNeighborDoodads.size + 2 * handlePositions.size)
    handlePositions.indices.foreach { i =>
      val c = handlePositions(i)
      val idx = 2 * (compilationResult.edgeNeighborDoodads.size + i)
      b1(idx) = w * c.x
      b1(idx + 1) = w * c.y
    }

    // TODO:  Do the QR decomposition during compilation rather than here.
    val result = Dcs_qrsol.cs_qrsol(0, compilationResult.A1, b1)
    val similarityTransformedVertices = b1.grouped(2).take(vertices.size).map { case Array(x, y) => Point(x, y) }.toIndexedSeq

    // Now adjust scale
    val b2x = new Array[Double](compilationResult.edgeNeighborDoodads.size + handlePositions.size)
    val b2y = new Array[Double](compilationResult.edgeNeighborDoodads.size + handlePositions.size)
    compilationResult.edgeNeighborDoodads.iterator.zipWithIndex.foreach {
      case (EdgeNeighborDoodad(Edge(i, j), neighborInfo, matGTGinvGT), edgeIdx) =>

        val (c, s) = {
          val xyCoords = (Iterable(i, j) ++ neighborInfo.indices)
            .map(similarityTransformedVertices(_).toList)
            .flatten
            .toArray
          
          val cs = matGTGinvGT.mult(new DenseVector(xyCoords), new DenseVector(2))
          (cs.get(0), cs.get(1))
        }

        val T = new DenseMatrix(2, 2)
        T.set(0, 0,  c);  T.set(0, 1, s)
        T.set(1, 0, -s);  T.set(1, 1, c)
        T.scale(1d / math.sqrt(c * c + s * s))

        val e = vertices(j) - vertices(i)

        val m = T.mult(new DenseVector(Array(e.x, e.y)), new DenseVector(2))
        b2x(edgeIdx) = m.get(0)
        b2y(edgeIdx) = m.get(1)
    }

    handlePositions.indices.foreach { i =>
      val c = handlePositions(i)
      val b2Idx = compilationResult.edgeNeighborDoodads.size + i
      b2x(b2Idx) = w * c.x
      b2y(b2Idx) = w * c.y
    }

    Dcs_qrsol.cs_qrsol(0, compilationResult.A2, b2x)
    Dcs_qrsol.cs_qrsol(0, compilationResult.A2, b2y)

    vertices.indices.map(i => Point(b2x(i), b2y(i)))
  }

  /*
   * This is the bottom portion of the matrix A1 that optimizes translation/rotation.
   */
  private[this] def buildMatrixC1(handleIDs: IndexedSeq[Int], w: Double): DenseMatrix = {
    val C1 = new DenseMatrix(2 * handleIDs.size, 2 * vertices.size)

    handleIDs.iterator.zipWithIndex.foreach {
      case (handleId, rowIdx) =>
        C1.set(2 * rowIdx, 2 * handleId, w)
        C1.set(2 * rowIdx + 1, 2 * handleId + 1, w)
    }
    C1
  }

  /*
   * This is the bottom portion of the matrix A2 that optimizes scale.
   */
  private[this] def buildMatrixC2(handleIDs: IndexedSeq[Int], w: Double): DenseMatrix = {
    val C2 = new DenseMatrix(handleIDs.size, vertices.size)

    handleIDs.iterator.zipWithIndex.foreach {
      case (handleId, row) =>
        C2.set(row, handleId, w)
    }
    C2
  }
}