package com.aylasoftware.bendaroni

import shapeless._
import shapeless.syntax.std.tuple._
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.Matrices
import scala.collection.JavaConverters._
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.csparsej.tdouble.Dcs_util
import edu.emory.mathcs.csparsej.tdouble.Dcs_entry
import edu.emory.mathcs.csparsej.tdouble.Dcs_compress
import edu.emory.mathcs.csparsej.tdouble.Dcs_qrsol
import no.uib.cipr.matrix.DenseVector

// TODO: Use sparse matrices everywhere
case class TriangulatedShape(
    vertices: IndexedSeq[Point],
    triangles: IndexedSeq[(Int, Int, Int)]) {

  case class Edge(i: Int, j: Int) {
    require(i < j)
  }

  val w = 1000d

  private[this] case class Triangle(i: Int, j: Int, k: Int)

  case class EdgeNeighborDoodad(
    e: Edge,
    neighborInfo: LeftRightNeighborInfo,
    GTGinvGT: DenseMatrix)

  case class RegistrationResult(L1: DenseMatrix, L2: DenseMatrix,
                                edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad])
  /*
   * Calculate the top of matrices A1 and A2.  Also calculate
   * matrix G products for each edge.
   */
  def register(): RegistrationResult = {
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

        EdgeNeighborDoodad(e, edgeNeighborInfo, GTGinvGT)
    }.toIndexedSeq

    val L1 = buildMatrixL1(edgeNeighborDoodads)
    val L2 = buildMatrixL2(edgeNeighborDoodads)

    RegistrationResult(L1, L2, edgeNeighborDoodads)
  }

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
    val Y8 = new DenseMatrix(2, 8)

    val X6 = new DenseMatrix(2, 6)
    X6.set(0, 0, -1); X6.set(0, 2, 1)
    X6.set(1, 1, -1); X6.set(1, 3, 1)
    val Y6 = new DenseMatrix(2, 6)

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

        def caseOneNeighbor(n: Int): Unit = {
          E.mult(matGTGinvGT, E6)
          val Y6 = X6.copy()
          Y6.add(-1, E6)

          // Unpack the coefficients and stuff them into the matrix L1.
          val row1 = 2 * edgeIdx
          L1.set(row1, 2 * i, Y6.get(0, 0))
          L1.set(row1, 2 * i + 1, Y6.get(0, 1))
          L1.set(row1, 2 * j, Y6.get(0, 2))
          L1.set(row1, 2 * j + 1, Y6.get(0, 3))
          L1.set(row1, 2 * n, Y6.get(0, 4))
          L1.set(row1, 2 * n + 1, Y6.get(0, 5))

          val row2 = row1 + 1
          L1.set(row2, 2 * i, Y6.get(1, 0))
          L1.set(row2, 2 * i + 1, Y6.get(1, 1))
          L1.set(row2, 2 * j, Y6.get(1, 2))
          L1.set(row2, 2 * j + 1, Y6.get(1, 3))
          L1.set(row2, 2 * n, Y6.get(1, 4))
          L1.set(row2, 2 * n + 1, Y6.get(1, 5))
        }

        neighborInfo match {
          case LeftAndRightNeighbors(l, r) =>
            E.mult(matGTGinvGT, E8)
            val Y8 = X8.copy()
            Y8.add(-1, E8)

            // Unpack the coefficients and stuff them into the matrix L1.
            val row1 = 2 * edgeIdx
            L1.set(row1, 2 * i, Y8.get(0, 0))
            L1.set(row1, 2 * i + 1, Y8.get(0, 1))
            L1.set(row1, 2 * j, Y8.get(0, 2))
            L1.set(row1, 2 * j + 1, Y8.get(0, 3))
            L1.set(row1, 2 * l, Y8.get(0, 4))
            L1.set(row1, 2 * l + 1, Y8.get(0, 5))
            L1.set(row1, 2 * r, Y8.get(0, 6))
            L1.set(row1, 2 * r + 1, Y8.get(0, 7))

            val row2 = row1 + 1
            L1.set(row2, 2 * i, Y8.get(1, 0))
            L1.set(row2, 2 * i + 1, Y8.get(1, 1))
            L1.set(row2, 2 * j, Y8.get(1, 2))
            L1.set(row2, 2 * j + 1, Y8.get(1, 3))
            L1.set(row2, 2 * l, Y8.get(1, 4))
            L1.set(row2, 2 * l + 1, Y8.get(1, 5))
            L1.set(row2, 2 * r, Y8.get(1, 6))
            L1.set(row2, 2 * r + 1, Y8.get(1, 7))

          case LeftNeighbor(l)  => caseOneNeighbor(l)
          case RightNeighbor(r) => caseOneNeighbor(r)
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

  // TODO:  Remove the naive matrices when things appear to work perfectly.
  case class CompilationResult(A1: Dcs, A2: Dcs, edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad])

  // TODO:  This should return something.
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

        val vi = similarityTransformedVertices(i)
        val vj = similarityTransformedVertices(j)

        // TODO: Clean up redundancy here.
        val (c, s) = neighborInfo match {
          case LeftAndRightNeighbors(l, r) =>
            val vl = similarityTransformedVertices(l)
            val vr = similarityTransformedVertices(r)
            val cs = matGTGinvGT.mult(new DenseVector(Array(vi.x, vi.y, vj.x, vj.y, vl.x, vl.y, vr.x, vr.y)), new DenseVector(2))
            (cs.get(0), cs.get(1))
          case LeftNeighbor(l) =>
            val vl = similarityTransformedVertices(l)
            val cs = matGTGinvGT.mult(new DenseVector(Array(vi.x, vi.y, vj.x, vj.y, vl.x, vl.y)), new DenseVector(2))
            (cs.get(0), cs.get(1))
          case RightNeighbor(r) =>
            val vr = similarityTransformedVertices(r)
            val cs = matGTGinvGT.mult(new DenseVector(Array(vi.x, vi.y, vj.x, vj.y, vr.x, vr.y)), new DenseVector(2))
            (cs.get(0), cs.get(1))
        }

        val T = new DenseMatrix(2, 2)
        T.set(0, 0, c)
        T.set(0, 1, s)
        T.set(1, 0, -s)
        T.set(1, 1, c)
        T.scale(1 / (c * c + s * s))

        val e = vj - vi

        val m = T.mult(new DenseVector(Array(e.x, e.y)), new DenseVector(2))
        b2x(edgeIdx) = m.get(0)
        b2y(edgeIdx) = m.get(1)
    }
    
    handlePositions.indices.foreach{i =>
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

  private[this] def buildMatrixC2(handleIDs: IndexedSeq[Int], w: Double): DenseMatrix = {
    val C2 = new DenseMatrix(handleIDs.size, vertices.size)

    handleIDs.iterator.zipWithIndex.foreach {
      case (handleId, row) =>
        C2.set(row, handleId, w)
    }
    C2
  }
}