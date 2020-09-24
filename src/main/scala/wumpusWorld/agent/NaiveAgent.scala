package wumpusWorld.agent

import scala.util.Random
import wumpusWorld.environment.{Action, Climb, Forward, Grab, Percept, Shoot, TurnLeft, TurnRight}

class NaiveAgent extends Agent {
  private val randGen = new Random()

  def nextAction(percept: Percept): Action  = {
    randGen.nextInt(6) match {
      case 0 => Forward
      case 1 => TurnLeft
      case 2 => TurnRight
      case 3 => Shoot
      case 4 => Grab
      case 5 => Climb
    }
  }
}
