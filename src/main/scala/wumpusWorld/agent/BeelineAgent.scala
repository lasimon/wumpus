package wumpusWorld.agent

import scala.util.Random
import wumpusWorld.environment.{Action, Climb, Forward, Grab, Percept, Shoot, TurnLeft, TurnRight}

class BeelineAgent extends Agent {
  private val randGen = new Random()

  private def doBeeline(beelineActionList: List[Action]): (Option[Action], List[Action]) =
    (beelineActionList.headOption, beelineActionList.tail)

  def nextAction(beliefState: BeelineBeliefState): (Action, Option[BeelineBeliefState])  = {
    if (beliefState.agentState.hasGold) {
      val (beelineActionOption, newBeelineActionList) = this.doBeeline(beliefState.beelineActionList)
      beelineActionOption match {
        case Some(action: Action) => (action, beliefState.copy(beelineActionList = newBeelineActionList))
        case None => (Climb, beliefState.copy(terminated = true))
      }
    } else {
      randGen.nextInt(6) match {
        case 0 => (Forward, beliefState.for)
        case 1 => (TurnLeft, xx)
        case 2 => (TurnRight, xx)
        case 3 => (Shoot, beliefState.copy(agentState = agentStatehaveArrow = false)
      }
    }
  }
}
