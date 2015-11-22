package com.aylasoftware.bendaroni.example

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import com.github.fommil.netlib.BLAS

object Demo extends SimpleSwingApplication {
  
  val sampleShape = ExampleTriangulatedShape.load()
  sampleShape.register()
  

  def top = new MainFrame {
    contents = new Panel {
      preferredSize = new Dimension(500, 500)
      listenTo(mouse.clicks)
      listenTo(mouse.moves)

      override def paint(g2d: Graphics2D): Unit = {
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
      }

      //      reactions += {
      //        case e: MousePressed =>
      //          if (state.pointsInProgress.isEmpty && !state.isMouseDragging) {
      //            state = pointsInProgressLens.set(state)(Seq(e.point))
      //            repaint()
      //          } else {
      //            
      //          }
      //        case e: MouseDragged =>
      //          state = (pointsInProgressLens ~ isMouseDraggingLens)
      //            .modify(state){case (pts, _) => (pts :+ e.point, true)}
      //          repaint()
      //        case e: MouseReleased =>
      //          state = (pointsInProgressLens ~ isMouseDraggingLens)
      //            .modify(state){case (pts, _) => (pts :+ e.point, false)}
      //          repaint()
      //      }
    }
  }
}