package com.aylasoftware.bendaroni

// TODO:  Edge representation could be better.
case class TriangulatedShape(
    points: IndexedSeq[Point], 
    triangles: IndexedSeq[(Int, Int, Int)])