package com.aylasoftware.bendaroni

class Edge private (val i: Int, val j: Int)

object Edge {
  def apply(i: Int, j: Int): Edge = new Edge(i min j, i max j)
  def unapply(e: Edge): Option[(Int, Int)] = Some((e.i, e.j))
}
