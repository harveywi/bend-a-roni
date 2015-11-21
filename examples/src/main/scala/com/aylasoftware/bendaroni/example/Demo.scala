package com.aylasoftware.bendaroni.example

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.Panel
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Dimension

object Demo extends SimpleSwingApplication {
  def top = new MainFrame {
    contents = new Panel {
      preferredSize = new Dimension(500, 500)
      override def paint(g2d: Graphics2D): Unit = {
        g2d.setColor(Color.orange)
        g2d.fillRect(0, 0, 50, 50)
      }
    }
  }
}