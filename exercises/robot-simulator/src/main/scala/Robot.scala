import Bearing._
import Commands.{Advance, Command, TurnLeft, TurnRight}

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {

  private def updateY(i: Int): (Int, Int) = (coordinates._1, coordinates._2 + i)
  private def updateX(i: Int): (Int, Int) = (coordinates._1 + i, coordinates._2)

  def turnRight: Robot = bearing match {
    case North => copy(bearing = East)
    case South => copy(bearing = West)
    case East  => copy(bearing = South)
    case West  => copy(bearing = North)
  }

  def turnLeft: Robot = bearing match {
    case North => copy(bearing = West)
    case South => copy(bearing = East)
    case East  => copy(bearing = North)
    case West  => copy(bearing = South)
  }

  def advance: Robot = bearing match {
    case North => copy(coordinates = updateY(1))
    case South => copy(coordinates = updateY(-1))
    case East  => copy(coordinates = updateX(1))
    case West  => copy(coordinates = updateX(-1))
  }

  def simulate(cmdStr: String): Robot = {
    Command.unapply(cmdStr).foldLeft(this) {
      case (robot, TurnLeft)  => robot.turnLeft
      case (robot, TurnRight) => robot.turnRight
      case (robot, Advance)   => robot.advance
    }
  }

}

object Commands {

  sealed abstract class Command

  case object TurnLeft extends Command
  case object TurnRight extends Command
  case object Advance extends Command

  object Command {
    def unapply(cmd: String): List[Command] = {
      cmd.map {
        case 'L' => TurnLeft
        case 'R' => TurnRight
        case 'A' => Advance
        case x   => throw new IllegalArgumentException(s"$x is not a valid command")
      }.toList
    }
  }

}

object Bearing {

  sealed trait Bearing

  case object North extends Bearing
  case object South extends Bearing
  case object East extends Bearing
  case object West extends Bearing

}
