package com.aylasoftware.bendaroni.example

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import com.github.fommil.netlib.BLAS
import scala.swing.event._
import javax.swing.SwingUtilities
import com.aylasoftware.bendaroni.Point
import com.aylasoftware.bendaroni.TriangulatedShape

object Demo extends SimpleSwingApplication {

  val handleRadius = 5

  val sampleShape = ExampleTriangulatedShape.load()
  val registrationResult = sampleShape.register()

  /*
   * All mutable state is accounted for right here. 
   */
  val handles = new scala.collection.mutable.ArrayBuffer[Int]
  var draggingHandle: Option[Int] = None
  var compilationResult: Option[sampleShape.CompilationResult] = None
  var newVertexPositions: Option[IndexedSeq[Point]] = None

  def top = new MainFrame {
    contents = new Panel {
      preferredSize = new Dimension(500, 500)
      listenTo(mouse.clicks)
      listenTo(mouse.moves)

      override def paint(g2d: Graphics2D): Unit = {
        g2d.setColor(Color.white)
        g2d.fillRect(0, 0, bounds.width, bounds.height)
        g2d.setColor(Color.black)

        sampleShape.triangles.foreach {
          case (i, j, k) =>
            val p1 = sampleShape.vertices(i)
            val p2 = sampleShape.vertices(j)
            val p3 = sampleShape.vertices(k)

            g2d.drawLine(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt)
            g2d.drawLine(p1.x.toInt, p1.y.toInt, p3.x.toInt, p3.y.toInt)
            g2d.drawLine(p2.x.toInt, p2.y.toInt, p3.x.toInt, p3.y.toInt)
        }

        def drawHandle(handleIdx: Int): Unit = {
          val p = sampleShape.vertices(handleIdx)
          val diameter = 2 * handleRadius + 1
          g2d.fillOval(p.x.toInt - handleRadius, p.y.toInt - handleRadius, diameter, diameter)
        }

        g2d.setColor(Color.orange)
        handles.foreach(drawHandle)

        g2d.setColor(Color.red)
        draggingHandle.foreach(drawHandle)

        g2d.setColor(Color.green)
        newVertexPositions.foreach { vertices =>
          sampleShape.triangles.foreach {
            case (i, j, k) =>
              val p1 = vertices(i)
              val p2 = vertices(j)
              val p3 = vertices(k)

              g2d.drawLine(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt)
              g2d.drawLine(p1.x.toInt, p1.y.toInt, p3.x.toInt, p3.y.toInt)
              g2d.drawLine(p2.x.toInt, p2.y.toInt, p3.x.toInt, p3.y.toInt)
          }
        }
      }

      reactions += {
        case e: MousePressed =>
          val p = Point(e.point.x, e.point.y)
          if (SwingUtilities.isRightMouseButton(e.peer)) {
            // Add a new handle.  Just choose the closest vertex.
            val closestVertexIdx = sampleShape.vertices.indices.minBy { i =>
              val q = sampleShape.vertices(i)
              p.distSq(q)
            }
            if (!handles.contains[Int](closestVertexIdx))
              handles += closestVertexIdx
            else
              handles -= closestVertexIdx

            if (!handles.isEmpty)
              compilationResult = Some(sampleShape.compile(handles, registrationResult))

            repaint()

          } else if (SwingUtilities.isLeftMouseButton(e.peer)) {
            // Did we manage to grab a handle?
            // draggingHandle
            draggingHandle = handles.iterator.find(i =>
              p.dist(sampleShape.vertices(i)) <= handleRadius + 1
            )

            repaint()
          }
        case e: MouseDragged =>
          if (SwingUtilities.isLeftMouseButton(e.peer)) {
            draggingHandle.zip(compilationResult).foreach {
              case (h, c) =>
                val handlePositions = handles.map {
                  case i if i == h => Point(e.point.x, e.point.y)
                  case i           => sampleShape.vertices(i)
                }
                newVertexPositions = Some(sampleShape.calculateNewVertexPositions(
                  handlePositions, c))
            }
            //            val handlePositions = handles.map{
            //              case i if i
            //            }
            //            sampleShape.calculateNewVertexPositions(
            //                handlePositions, compilationResult)
          }
          repaint()
        case e: MouseReleased =>
          draggingHandle = None
          repaint()
      }
    }
  }
}