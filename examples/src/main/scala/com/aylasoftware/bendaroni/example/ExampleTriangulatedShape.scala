package com.aylasoftware.bendaroni.example

import java.io.File
import com.aylasoftware.bendaroni._
import scala.io.Source

object ExampleTriangulatedShape {
  private[this] def getFile(path: String): File =
    new File(new File(System.getProperty("user.dir")), path)

  def load(): TriangulatedShape = {
    val nodeFile = getFile("data/A.1.node")
    val polyFile = getFile("data/A.1.poly")
    val eleFile = getFile("data/A.1.ele")

    val scaleFactor = 300d
    val vertices = {
      val vertices = loadVertices(nodeFile)
      val minX = vertices.minBy(_.x).x
      val maxX = vertices.maxBy(_.x).x
      val minY = vertices.minBy(_.y).y
      val maxY = vertices.maxBy(_.y).y
      val rangeX = maxX - minX
      val rangeY = maxY - minY
      
      vertices.map{p =>
        val nx = (p.x - minX) / rangeX * scaleFactor
        val ny = scaleFactor - (p.y - minY) / rangeY * scaleFactor
        
        Point(nx, ny)
      }
    }
    
    val triangles = loadTriangles(eleFile)
    
    TriangulatedShape(vertices, triangles)
  }

  private[this] def loadVertices(nodeFile: File): IndexedSeq[Point] = {
    Source.fromFile(nodeFile).getLines.drop(1).collect {
      case line if !line.startsWith("#") =>
        line.trim.split("\\s+").slice(1, 3).map(_.toDouble) match {
          case Array(x, y) => Point(x, y)
        }
    }.toIndexedSeq
  }
  
  private[this] def loadTriangles(eleFile: File): IndexedSeq[Triangle] = {
    Source.fromFile(eleFile).getLines.drop(1).collect {
      case line if !line.startsWith("#") =>
        line.trim.split("\\s+").drop(1).map(_.toInt - 1) match {
          case Array(i, j, k) => Triangle(i, j, k)
        }
    }.toIndexedSeq
  }
}