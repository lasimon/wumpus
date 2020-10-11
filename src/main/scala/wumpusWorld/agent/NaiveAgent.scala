package wumpusWorld.agent

import scala.util.Random
import wumpusWorld.environment.{Action, Climb, Forward, Grab, Percept, Shoot, TurnLeft, TurnRight}

class NaiveAgent extends Agent {
  private val randGen = new Random()

  def nextAction(percept: Percept): (Agent, Action)  = {
    randGen.nextInt(6) match {
      case 0 => (this, Forward)
      case 1 => (this, TurnLeft)
      case 2 => (this, TurnRight)
      case 3 => (this, Shoot)
      case 4 => (this, Grab)
      case 5 => (this, Climb)
    }
  }
}
