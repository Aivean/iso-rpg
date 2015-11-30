package com.aivean.isorpg.game

import scala.collection.mutable

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-15
  */
object Utils {

  def path(start: Point, target: Point,
           canMove: (Point, Point) => Boolean): Option[List[Point]] = {

    val q = new mutable.Queue[Point]()
    val map = mutable.Map[Point, Point]()

    var moves = 0

    map += (start -> start)
    q.enqueue(start)

    while (q.nonEmpty && moves < 500) {
      moves += 1
      val p = q.dequeue()
      if (p == target) {
        var res = List(p)
        var c = p
        while (c != start) {
          c = map(c)
          res = c :: res
        }
        return Some(res)
      } else {
        (-1 to 1).flatMap(z => List(
          Point(-1, 0, z),
          Point(1, 0, z),
          Point(0, -1, z),
          Point(0, 1, z)
        )).map { case Point(x, y, z) => Point(x + p.x, y + p.y, z + p.z) }
          .filterNot(map.contains)
          .filter(p1 => canMove(p, p1))
          .sortBy(_.distTo(target))
          .foreach {
            p1 =>
              map += (p1 -> p)
              q.enqueue(p1)
          }
      }
    }

    None
  }

  def uuid = java.util.UUID.randomUUID.toString
}
