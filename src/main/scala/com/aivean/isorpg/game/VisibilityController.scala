package com.aivean.isorpg.game

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import archery.{Entry, RTree}
import com.aivean.isorpg.game.chars.Player

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-12-05
  */
class VisibilityController private extends Actor with ActorLogging {
  import VisibilityController._

  val index = new Index

  def notifyAboutStateUpdate(uuids:TraversableOnce[String]): Unit = {
    // also include yourself!
    uuids.foreach(uuid => self ! UpdatePlayersVisible(uuid))
  }

  def receive = LoggingReceive {
    case PlayerAdded(CharCommons(uuid, p, sprite), actor, w, h) =>
      val vis = new PlayerVisual(uuid, p, actor, w, h, sprite, Set())
      index.add(vis)
      notifyAboutStateUpdate(index.viewersOf(vis.uuid).filter(_.canSee(vis)).map(_.uuid))

    case NPCAdded(CharCommons(uuid, p, sprite)) =>
      val vis = new NpcVisual(uuid, p, sprite)
      index.add(vis)
      notifyAboutStateUpdate(index.viewersOf(vis.uuid).filter(_.canSee(vis)).map(_.uuid))

    case UpdatePlayersVisible(uuid) => index.find(uuid) match {
      case Some(player:PlayerVisual) =>
        val visible = index.viewField(uuid).filter(player.canSee)
        log.debug("FOV:" + visible.map(v => v.p + " " + v.uuid).mkString(", "))

        player.visible.diff(visible.map(_.uuid).toSet).foreach { uuid =>
          player.visible -= uuid
          player.actor ! Player.PlayerRemoved(uuid)
        }

        // not visible before
        visible.filterNot(v => player.visible.contains(v.uuid)).foreach { v =>
          player.visible += v.uuid
          player.actor ! Player.PlayerAdded(v.uuid, v.p, v.sprite)
        }

      case _ =>
    }

    case CharPositionChanged(uuid, to) => index.find(uuid).foreach { movingChar =>
      val oldViewers = index.viewersOf(uuid).filter(_.visible.contains(uuid)).map(_.uuid).toSet
      index.remove(uuid)
      movingChar.p = to
      index.add(movingChar)
      notifyAboutStateUpdate(index.viewersOf(uuid).filter(_.canSee(movingChar)).map(_.uuid).toSet ++ oldViewers)
    }

    case CharMoving(uuid, to, ts) =>
      index.viewersOf(uuid).filter(v => v.visible.contains(uuid))
        .foreach(v => v.actor ! Player.PlayerMoved(uuid, ts, to))

    case CharRemoved(uuid) =>
      index.viewersOf(uuid).filter(_.visible.contains(uuid)).foreach { v =>
        v.visible -= uuid
        v.actor ! Player.PlayerRemoved(uuid)
      }
      index.remove(uuid)


  }
}

object VisibilityController {

  def props = Props(new VisibilityController)

  trait CharVisual {
    def uuid: String
    var p: Point
    def sprite: String
  }

  class NpcVisual(val uuid:String, override var p:Point, val sprite:String) extends CharVisual


  class PlayerVisual(val uuid: String,
                      override var p: Point,
                      val actor: ActorRef,
                      val w: Int,
                      val h: Int,
                      val sprite: String,
                      var visible: Set[String]) extends CharVisual {


    def canSee(v:CharVisual) = {
      val thisPos = p.project
      val otherPos = v.p.project

      math.abs(thisPos.x - otherPos.x) < w / 2 &&
        math.abs(thisPos.y - otherPos.y) < h / 2
    }
  }

  class Index {
    private var uuids = Map[String, Wrapper]()
    private var visualFields = RTree[PlayerWrapper]()
    private var posIdx = RTree[Wrapper]()

    private sealed trait Wrapper {
      def vis:CharVisual
      val uuid = vis.uuid

      val posIdxEntry = Entry(archery.Point(vis.p.x, vis.p.y), this)
      val visSearchKey = archery.Box(vis.p.x, vis.p.y, vis.p.x, vis.p.y)

      def add(): Unit = {
        uuids += (uuid -> this)
        posIdx = posIdx.insert(posIdxEntry)
      }

      def remove():Unit = {
        uuids -= uuid
        posIdx = posIdx.remove(posIdxEntry)
      }
    }

    private sealed class NPCWrapper(val vis:CharVisual) extends Wrapper

    private sealed class PlayerWrapper(val vis:PlayerVisual) extends Wrapper {
      val posSearchKey = archery.Box(vis.p.x - vis.w, vis.p.y - vis.h,
        vis.p.x + vis.w, vis.p.y + vis.h)
      val visEntry = Entry(posSearchKey, this)

      override def add(): Unit = {
        super.add()
        visualFields = visualFields.insert(visEntry)
      }

      override def remove(): Unit = {
        super.remove()
        visualFields = visualFields.remove(visEntry)
      }
    }

    def find(uuid:String) = uuids.get(uuid).map(_.vis)

    def add(vis:CharVisual): Unit = if (!uuids.contains(vis.uuid)) vis match {
        case p:PlayerVisual => new PlayerWrapper(p).add()
        case npc => new NPCWrapper(npc).add()
      }

    def remove(uuid:String) = uuids.get(uuid).foreach(_.remove())

    def viewersOf(uuid:String) = uuids.get(uuid).toSeq.flatMap { w =>
      visualFields.searchIntersection(w.visSearchKey).map(_.value.vis)
    }

    def viewField(uuid:String) = uuids.get(uuid).toSeq.collect { case p:PlayerWrapper =>
      posIdx.search(p.posSearchKey).map(_.value.vis)
    }.flatten
  }

  case class CharCommons(uuid:String, p:Point, sprite:String)

  // messages
  case class PlayerAdded(commons:CharCommons, actor:ActorRef, w:Int, h:Int)
  case class NPCAdded(commons:CharCommons)
  case class UpdatePlayersVisible(uuid: String)
  case class CharMoving(uuid:String, to:Point, ts:Long)
  case class CharPositionChanged(uuid:String, to:Point)
  case class CharRemoved(uuid:String)

}
