package com.aylasoftware.bendaroni

case class Point(x: Double, y: Double) {
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def length = Math.sqrt(x * x + y * y)
  def dist(p: Point) = (this - p).length
  def dot(p: Point) = x * p.x + y * p.y
  def determinant(p: Point) = x * p.y - y * p.x
}