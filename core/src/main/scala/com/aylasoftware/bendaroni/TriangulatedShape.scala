package com.aylasoftware.bendaroni

import shapeless._
import shapeless.syntax.std.tuple._
import scala.collection.JavaConverters._
import org.aylasoftware.csparse.doubles.Dcs_compress
import org.aylasoftware.csparse.doubles.Dcs_common.Dcs
import org.aylasoftware.csparse.doubles.Dcs_util
import org.aylasoftware.csparse.doubles.Dcs_entry
import org.aylasoftware.csparse.doubles.Dcs_qrsol
import org.aylasoftware.csparse.doubles.Dcs_transpose
import org.aylasoftware.csparse.doubles.Dcs_multiply
import org.aylasoftware.csparse.doubles.Dcs_add
import org.aylasoftware.csparse.doubles.Dcs_gaxpy

case class TriangulatedShape(vertices: IndexedSeq[Point], triangles: IndexedSeq[Triangle]) {

  val w = 1000d

  sealed abstract class NeighborInfo(val indices: Int*) {
    def points: Iterable[Point] = indices.map(vertices)
  }
  case class LeftAndRightNeighbors(l: Int, r: Int) extends NeighborInfo(l, r)
  case class LeftNeighbor(l: Int) extends NeighborInfo(l)
  case class RightNeighbor(r: Int) extends NeighborInfo(r)

  case class EdgeNeighborDoodad(e: Edge, neighborInfo: NeighborInfo, GTGinvGT: Dcs)

  case class CompilationResult(A1: Dcs, A2: Dcs, edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad])
  case class RegistrationResult(L1: Dcs, L2: Dcs, edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad])

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

        val G = createMatrix(2 * pts.size, 4)(
          pts.iterator.zipWithIndex.flatMap {
            case (p, i) =>
              val row = 2 * i
              Seq(
                MatrixEntry(row, 0, p.x), MatrixEntry(row, 1, p.y), MatrixEntry(row, 2, 1),
                MatrixEntry(row + 1, 0, p.y), MatrixEntry(row + 1, 1, -p.x), MatrixEntry(row + 1, 3, 1)
              )
          }
        )

        val GT = Dcs_transpose.cs_transpose(G, true)

        val GTG = Dcs_multiply.cs_multiply(GT, G)

        val GTGinv = {
          // We have to do each column independently.  Ugh.
          createMatrix(GTG.n, GTG.n)((0 until GTG.m).iterator.flatMap { row =>
            val y = new Array[Double](G.n)
            y(row) = 1

            Dcs_qrsol.cs_qrsol(0, GTG, y)

            y.indices.map(col => MatrixEntry(row, col, y(col)))
          })
        }

        val GTGinvGT = Dcs_multiply.cs_multiply(GTGinv, GT)

        val GTGinvGT_top = createMatrix(2, G.m)(
          (0 until GTGinvGT.n).iterator.flatMap { c =>
            Seq(
              MatrixEntry(0, c, GTGinvGT.get(0, c)),
              MatrixEntry(1, c, GTGinvGT.get(1, c))
            )
          }
        )

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
  private[this] def buildMatrixL1(edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad]): Dcs = {
    val L1 = Dcs_util.cs_spalloc(2 * edgeNeighborDoodads.size, 2 * vertices.size, 1, true, true)

    val X8 = createMatrix(2, 8)(Iterator(
      MatrixEntry(0, 0, -1), MatrixEntry(0, 2, 1),
      MatrixEntry(1, 1, -1), MatrixEntry(1, 3, 1)
    ))

    val X6 = createMatrix(2, 6)(Iterator(
      MatrixEntry(0, 0, -1), MatrixEntry(0, 2, 1),
      MatrixEntry(1, 1, -1), MatrixEntry(1, 3, 1)
    ))

    edgeNeighborDoodads.iterator.zipWithIndex.foreach {
      case (EdgeNeighborDoodad(Edge(i, j), neighborInfo, matGTGinvGT), edgeIdx) =>
        val vi = vertices(i)
        val vj = vertices(j)
        val eVec = vj - vi

        val E = createMatrix(2, 2)(Iterator(
          MatrixEntry(0, 0, eVec.x), MatrixEntry(0, 1, eVec.y),
          MatrixEntry(1, 0, eVec.y), MatrixEntry(1, 1, -eVec.x)
        ))

        val (outXtemp) = neighborInfo match {
          case _: LeftAndRightNeighbors => X8.copy
          case _                        => X6.copy
        }

        val outE = Dcs_multiply.cs_multiply(E, matGTGinvGT)

        val outX = Dcs_add.cs_add(outXtemp, outE, 1, -1)

        // Unpack the coefficients and stuff them into the matrix L1
        val row1 = 2 * edgeIdx
        val row2 = row1 + 1

        (Iterable(i, j) ++ neighborInfo.indices).iterator.zipWithIndex.foreach {
          case (i, j) =>
            assert(Dcs_entry.cs_entry(L1, row1, 2 * i, outX.get(0, 2 * j)) != null)
            assert(Dcs_entry.cs_entry(L1, row1, 2 * i + 1, outX.get(0, 2 * j + 1)) != null)

            assert(Dcs_entry.cs_entry(L1, row2, 2 * i, outX.get(1, 2 * j)) != null)
            assert(Dcs_entry.cs_entry(L1, row2, 2 * i + 1, outX.get(1, 2 * j + 1)) != null)
        }
    }
    Dcs_compress.cs_compress(L1)
  }

  /*
   * L2 is the top portion of the matrix used for optimizing scale. 
   */
  private[this] def buildMatrixL2(edgeNeighborDoodads: IndexedSeq[EdgeNeighborDoodad]): Dcs = {
    val L2 = Dcs_util.cs_spalloc(edgeNeighborDoodads.size, vertices.size, 1, true, true)

    edgeNeighborDoodads.iterator.zipWithIndex.foreach {
      case (EdgeNeighborDoodad(Edge(i, j), _, _), edgeIdx) =>
        assert(Dcs_entry.cs_entry(L2, edgeIdx, i, -1) != null)
        assert(Dcs_entry.cs_entry(L2, edgeIdx, j, 1) != null)
    }
    Dcs_compress.cs_compress(L2)
  }

  def compile(handleIDs: IndexedSeq[Int], registrationResult: RegistrationResult): CompilationResult = {
    val L1 = registrationResult.L1
    val L2 = registrationResult.L2

    val C1 = buildMatrixC1(handleIDs, w)
    val C2 = buildMatrixC2(handleIDs, w)

    // Copy L1 and C1 into A1.
    val A1 = Dcs_util.cs_spalloc(0, 0, 1, true, true)
    L1.entries.foreach(e => assert(Dcs_entry.cs_entry(A1, e.row, e.column, e.value) != null))

    C1.entries.foreach(e => assert(Dcs_entry.cs_entry(A1, e.row + L1.m, e.column, e.value) != null))

    val A1_compressed = Dcs_compress.cs_compress(A1)

    // Copy L2 and C2 into A2.
    val A2 = Dcs_util.cs_spalloc(0, 0, 1, true, true)
    L2.entries.foreach(e => assert(Dcs_entry.cs_entry(A2, e.row, e.column, e.value) != null))

    C2.entries.foreach(e => assert(Dcs_entry.cs_entry(A2, e.row + L2.m, e.column, e.value) != null))

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

          val cs = new Array[Double](2)
          Dcs_gaxpy.cs_gaxpy(matGTGinvGT, xyCoords, cs)
          (cs(0), cs(1))
        }

        val scaleNorm = math.sqrt(c * c + s * s)
        val cs = c / scaleNorm
        val ss = s / scaleNorm
        val T = createMatrix(2, 2)(Iterator(
          MatrixEntry(0, 0, cs), MatrixEntry(0, 1, ss),
          MatrixEntry(1, 0, -ss), MatrixEntry(1, 1, cs)
        ))

        val e = vertices(j) - vertices(i)

        val m = new Array[Double](2)
        Dcs_gaxpy.cs_gaxpy(T, Array(e.x, e.y), m)
        b2x(edgeIdx) = m(0)
        b2y(edgeIdx) = m(1)
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
  private[this] def buildMatrixC1(handleIDs: IndexedSeq[Int], w: Double): Dcs =
    createMatrix(2 * handleIDs.size, 2 * vertices.size)(
      handleIDs.iterator.zipWithIndex.flatMap {
        case (handleId, rowIdx) =>
          Iterator(
            MatrixEntry(2 * rowIdx, 2 * handleId, w),
            MatrixEntry(2 * rowIdx + 1, 2 * handleId + 1, w)
          )
      }
    )

  /*
   * This is the bottom portion of the matrix A2 that optimizes scale.
   */
  private[this] def buildMatrixC2(handleIDs: IndexedSeq[Int], w: Double): Dcs =
    createMatrix(handleIDs.size, vertices.size)(
      handleIDs.iterator.zipWithIndex.map {
        case (handleId, row) =>
          MatrixEntry(row, handleId, w)
      }
    )

  private[this] case class MatrixEntry(row: Int, column: Int, value: Double)
  private[this] def createMatrix(m: Int, n: Int)(data: Iterator[MatrixEntry]): Dcs = {
    val A = Dcs_util.cs_spalloc(m, n, 1, true, true)
    data.filter(_.value != 0).foreach(e => assert(Dcs_entry.cs_entry(A, e.row, e.column, e.value) != null))

    Dcs_compress.cs_compress(A)
  }
}