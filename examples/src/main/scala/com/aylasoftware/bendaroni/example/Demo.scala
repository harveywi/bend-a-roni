package com.aylasoftware.bendaroni.example

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D

import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication

object Demo extends SimpleSwingApplication {
  
  val sampleShape = ExampleTriangulatedShape.load()

  def top = new MainFrame {
    contents = new Panel {
      preferredSize = new Dimension(500, 500)
      listenTo(mouse.clicks)
      listenTo(mouse.moves)

      override def paint(g2d: Graphics2D): Unit = {
        g2d.setColor(Color.black)

        val minX = sampleShape.points.minBy(_.x).x
        val maxX = sampleShape.points.maxBy(_.x).x
        val minY = sampleShape.points.minBy(_.y).y
        val maxY = sampleShape.points.maxBy(_.y).y

        val rangeX = if (maxX - minX > 0) maxX - minX else 1d
        val rangeY = if (maxY - minY > 0) maxY - minY else 1d

        sampleShape.triangles.foreach {
          case (i, j, k) =>
            val p1 = sampleShape.points(i)
            val p2 = sampleShape.points(j)
            val p3 = sampleShape.points(k)
            
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