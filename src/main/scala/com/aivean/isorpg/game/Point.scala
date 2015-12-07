package com.aivean.isorpg.game

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-26
  */
case class Point(x: Int, y: Int, z: Int) {
  import Point._
  def down = this.copy(z = z - 1)
  def up = this.copy(z = z + 1)

  def west = this.copy(x = x - 1)
  def east = this.copy(x = x + 1)
  def north = this.copy(y = y - 1)
  def south = this.copy(y = y + 1)

  def distTo(p:Point) =
    math.sqrt((p.x - x) * (p.x - x) + (p.y - y) * (p.y - y) + (p.z - z) * (p.z - z))

  def moved(x: Int = 0, y: Int = 0, z: Int = 0) =
    copy(this.x + x, this.y + y, this.z + z)

  /**
    * neighbors in particular order:
    * S SW W NW N NE E SE
    */
  def adjFlatAll =
    Seq(south, south.west, west, north.west,
      north, north.east, east, south.east)

  def adjFlatCross = Seq(west, east, north, south)

  def closeProximity = adjFlatCross ++ adjFlatCross.map(_.up) ++ adjFlatCross.map(_.down)

  def chunk = ((x >> chunkBits).toLong << (32 - chunkBits)) | (y.toLong >> chunkBits)

  def project = Point2D(
    (Point.this.x - Point.this.y) * transform._1,
    (Point.this.x + Point.this.y) * transform._2 - Point.this.z
  )
}

object Point {
  val chunkBits = 2
  val chunkSize = 1 << chunkBits

  val projectionAngle = math.atan(0.5)
  val transform = (math.cos(projectionAngle), math.sin(projectionAngle))

  case class Point2D(x:Double, y:Double)
}