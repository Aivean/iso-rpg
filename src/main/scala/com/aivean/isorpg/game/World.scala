package com.aivean.isorpg.game

import java.util.concurrent.ForkJoinPool

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import com.aivean.isorpg.game.Tiles.TileType
import com.aivean.isorpg.game.VisibilityController.CharCommons
import com.aivean.isorpg.game.chars._
import xitrum.Config

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-13
  */

  class World private extends Actor with ActorLogging {
    import World._
    import akka.pattern.pipe
    implicit val ec = context.dispatcher

    val visContr = context.actorOf(VisibilityController.props, "visContrl")

    val worldSize = 100
    var world = new Terrain(TerrainGen.apply(worldSize), Set())

    var players = Map[ActorRef, CharView]()
    var npcs = Map[ActorRef, CharView]()
    var indexByUuid = Map[String, CharView]()
    def allChars = (players.view ++ npcs).map(_._2)
    def findChar(actor:ActorRef) = players.get(actor).orElse(npcs.get(actor))

    for(i <- 1 to 100) {
        val fp = genFreePoint
        world = world.addOccupant(fp)
        val uuid = Utils.uuid
        val a = context.actorOf(Monster.props(fp), "poring-" + uuid)
        val view = new CharView(uuid, CharState.Standing(fp), a, "poring")
        npcs += (a -> view)
        indexByUuid += (uuid -> view)
        visContr ! VisibilityController.NPCAdded(CharCommons(view.uuid, view.state.p, view.sprite))
      }

    def zShift(p:Point) = world.map.get(p.down).map(_.height - 1f).getOrElse(0f)

    def genFreePoint = {
      val p = Random.shuffle(players.map(_._2.state.p)).headOption.getOrElse(Random.shuffle(world.map.keys.toSeq).head)
      Random.shuffle(world.map.keys.toSeq.filter(_.distTo(p) <= 10).map(_.up).filter(world.freeToMoveTo)).head
    }

    def receive = LoggingReceive {
      case ClientConnected(uuid) =>
        val client = sender
        val char = {
          val pos = genFreePoint
          val actorRef = context.actorOf(Player.props(pos, client), "player-" + uuid)
          new CharView(uuid, CharState.Standing(pos), actorRef, "player")
        }
        world = world.addOccupant(char.state.p)
        players += (char.actor -> char)
        indexByUuid += (char.uuid -> char)

        visContr ! VisibilityController.PlayerAdded(CharCommons(char.uuid, char.state.p, char.sprite),
          char.actor, 25, 21)

      case AdminCommand(cmd, player) =>
        val NEIGHTBOURS_PATTERN = """tileNeighbors[^\d-]+(-?[\d]+)[^\d-]+(-?[\d]+)[^\d-]+(-?[\d]+)""".r
        cmd match {
        case "generateWorld" =>
          //FIXME broken! need to reposition chars correctly
          world = new Terrain(TerrainGen.apply(worldSize), world.occupied)
          players.foreach(_._2.actor ! Player.ServerMessage("World Regenerated!"))
        case "location" =>
          players.get(player) foreach { player =>
            player.actor ! Player.ServerMessage(player.state.p.toString)
          }
        case NEIGHTBOURS_PATTERN(x, y, z) =>
          try {
            val p = Point(x.toInt, y.toInt, z.toInt)
            val result = ListMap("point" -> p, "s" -> p.south, "sw" -> p.south.west, "w" -> p.west, "nw" -> p.north
              .west, "n" -> p.north, "ne" -> p.north.east, "e" -> p.east, "se" -> p.south.east, "up" -> p.up,
              "down" -> p.down)
            .mapValues(world.map.get(_).map(_.tile).getOrElse("")).map(x => x._1 + ":"+x._2).mkString(", ")
            player ! Player.ServerMessage(result)
          } catch {case e: Throwable => player ! Player.ServerMessage("Error: " + e.getMessage)}

        case x => player ! Player.ServerMessage("unknown command: " + x)
      }

      case PlayerWantsToMove(targetP) =>
        log.debug(s"want to move to $targetP")
        findChar(sender).filter(_.state.isInstanceOf[CharState.Standing])
          .foreach { movingPlayer =>

            log.debug(s"found player $movingPlayer ${movingPlayer.state}")

            val world = this.world //closing world
            Future {
              { // path
                val startP = movingPlayer.state.p
                def calcPath(pts: Set[Point]) = Utils.path(startP, pts, { (p, p1) => world.freeToMoveTo(p1) })
                calcPath(Set(targetP)).orElse(targetP.closeProximity.toSet match {
                  case x if !x.contains(startP) => x.filter(world.freeToMoveTo) match {
                    case Seq() => None
                    case x => calcPath(x)
                  }
                  case _ => None
                })
              }.foreach { path =>
                  log.debug(s"found path $path")
                  self ! CharAdvance(movingPlayer.actor, targetP, path)
              }
            } (pathCalculationEc)
          }

      case CharAdvance(charActor, targetP, path) =>
        findChar(charActor).foreach { movingChar =>
          path.slice(1, 2).headOption.filter(world.freeToMoveTo).foreach { nextPoint =>
            log.debug(s"moving to next point $nextPoint")
            val stepTime = 400 milliseconds

            val ts = System.currentTimeMillis() + stepTime.toMillis
            world = world.moveOccupant(movingChar.state.p, nextPoint)
            movingChar.state = CharState.MovingTo(nextPoint, ts)

            visContr ! VisibilityController.CharMoving(movingChar.uuid, ts, nextPoint, zShift(nextPoint))

            context.system.scheduler.scheduleOnce(stepTime, self,
              UpdateCharState(movingChar.actor))
          }
        }

      case PlayerWantsToAttack(uuid) => findChar(sender)
        .filter(_.state.isInstanceOf[CharState.Standing]).foreach { player =>
        indexByUuid.get(uuid) match {
          case Some(target) if target.state.p.closeProximity.contains(player.state.p) =>
            sender ! Player.ServerMessage("Attack!")
            val attackTime = 300 milliseconds
            val time = System.currentTimeMillis()
            player.state = CharState.Attacking(target.uuid, player.state.p,  time + attackTime.toMillis)
            visContr ! VisibilityController.CharAttacking(player.uuid, target.uuid, attackTime.toMillis)
            context.system.scheduler.scheduleOnce(attackTime, self,
              UpdateCharState(player.actor))

          case Some(target) =>
            self forward PlayerWantsToMove(target.state.p)

          case None => sender ! StatePolling.AttackTargetDoesNotExist(uuid)
        }
      }

      case msg@UpdateCharState(playerActor) =>
        findChar(playerActor).foreach { player =>
          log.debug("updating moving state")
          val time = System.currentTimeMillis()
          player.state match {
            case CharState.Standing(_) =>

            case x:CharState.Timed if x.endTs > time =>
              log.debug(s"action $x is not yet completed, rescheduling")
              context.system.scheduler.scheduleOnce((x.endTs - time) milliseconds, self, msg)

            case CharState.MovingTo(p, ts) =>
              player.state = CharState.Standing(p)
              log.debug(s"arrived at $p")
              player.actor ! StatePolling.ArrivedAt(p)
              visContr ! VisibilityController.CharPositionChanged(player.uuid, p)

            case CharState.Attacking(uuid, p, ts)  =>
              player.state = CharState.Standing(p)
              log.debug(s"hit!")

            case _ =>
          }
        }

      case ChunksRequest(chIds) =>
        sender ! PlayerChunksController.TilesAdded(
          chIds.toList.flatMap(id => world.chunks.get(id).map(c => (id, c))).toMap)

      case SurroundingsRequest(point, dist) =>
        val world = this.world
        Future {
          val q = mutable.Queue[Point](point)
          var res = Map[Point, TileType]()
          while (q.nonEmpty) {
            val p = q.dequeue()
            val toAdd = (p.adjFlatAll :+ p.up :+ p.down)
              .filterNot(res.contains).filter(_.distTo(point) <= dist)
              .flatMap(p => world.map.get(p).map(t => (p, t))).toMap
            res ++= toAdd
            toAdd.keys.foreach(q.enqueue(_))
          }
          Monster.SurroundingsView(res)
        }(pathCalculationEc) pipeTo sender

      //TODO delegate broadcast functionality to other dedicated actor

      case PlayerTalking(msg) => players.get(sender).foreach { player =>
        players.foreach(_._2.actor ! Player.PlayerTalking(player.uuid, msg))
      }

      case ClientDisconnected =>
        players.get(sender).foreach { p =>
          players -= p.actor
          indexByUuid -= p.uuid
          world = world.removeOccupant(p.state.p)
          visContr !VisibilityController.CharRemoved(p.uuid)
        }
    }
  }

  object World {
    //def props = Props(new World)
    val world = Config.actorSystem.actorOf(Props(new World), "world")

    private val pathCalculationEc = ExecutionContext.fromExecutorService(new ForkJoinPool(8))

    // private messages
    private case class UpdateCharState(player:ActorRef)

    private case class CharAdvance(player:ActorRef, targetP: Point, path:List[Point])

    // public messages
    case class ClientConnected(uuid: String)

    case object ClientDisconnected

    case class AdminCommand(cmd:String, p:ActorRef)

    case class PlayerWantsToMove(p: Point)

    case class PlayerWantsToAttack(uuid:String)

    case class PlayerTalking(msg:String)

    case class ChunksRequest(ids:Set[Long])

    case class SurroundingsRequest(p:Point, dist:Float)

    class Terrain private (val map:Map[Point, TileType], val occupied:Set[Point],
                           val chunks:Map[Long, Map[Point,TileType]]) {
      def this(map: Map[Point, TileType], occupied: Set[Point]) = this(map, occupied, map.groupBy(_._1.chunk))

      //private def changeMap()
      private def changeOccupancy(func:Set[Point] => Set[Point]) = new Terrain(map, func(occupied), chunks)

      def moveOccupant(from:Point, to:Point) = changeOccupancy(_ - from + to)
      def addOccupant(to:Point) = changeOccupancy(_ + to)
      def removeOccupant(from:Point) = changeOccupancy(_ - from)

      def freeToMoveTo(p:Point) =
        map.get(p.down).exists(_.standable) &&
          !map.get(p).exists(!_.passable) &&
          !occupied.contains(p)
    }

    class CharView(
                    val uuid: String,
                    var state: CharState.Type,
                    val actor: ActorRef,
                    val sprite: String
                    )

    object CharState {
      sealed trait Type {
        def p:Point
      }

      sealed trait Timed {
        def endTs:Long
      }

      case class Standing(p: Point) extends Type

      case class MovingTo(p: Point, endTs: Long) extends Type with Timed

      case class Attacking(uuid:String, p:Point, endTs: Long) extends Type with Timed
    }
  }
