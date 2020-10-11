package wumpusWorld.agent

import scala.util.Random
import wumpusWorld.environment.{Action, AgentState, Climb, Coords, Forward, Grab, Percept, Shoot, TurnLeft, TurnRight}

final case class BeelineAgent private(
                            gridWidth: Int,
                            gridHeight: Int,
                            agentState: AgentState,
                            safeLocations: Set[Coords],
                            beelineActionList: List[Action]
                          ) extends Agent {

  private val randGen = new Random()

  private def constructBeelinePlan: List[Action] = ??? // create a list of move actions to get out quickly and safely
  // do this by creating a directed graph with safe locations as nodes and edges pointing possible ways back to the start square
  // then find the shortest path

  def nextAction(percept: Percept): (BeelineAgent, Action)  = {
    if (agentState.hasGold) { // we're in the endgame
      if (agentState.location == Coords(0,0)) { // we have a winner
        (this, Climb)
      } else { // we have the gold but aren't home yet. Construct a plan if we don't have one.
        val beelinePlan: List[Action] = if (beelineActionList.isEmpty) constructBeelinePlan else beelineActionList
        val beelineAction = beelinePlan.head // take the action at the head of the list of remaining actions
        (
          this.copy(
            agentState = agentState.applyMoveAction(beelineAction, gridWidth, gridHeight),
            beelineActionList = beelinePlan.tail // set the beelineActionList to the actions remaining to be done
          ),
          beelineAction
        )
      }
    } else if (percept.glitter) { // we don't have the gold but we're in the room with it
      (this.copy(agentState = agentState.copy(hasGold = true)), Grab)
    } else { // we don't have the gold and aren't in the gold room so choose a random move or shoot action
      randGen.nextInt(4) match {
        case 0 => {
          val newAgentState = agentState.forward(gridWidth, gridHeight)
          val newSafeLocations: Set[Coords] = safeLocations + newAgentState.location // move may not actually be safe, but if not agent will be dead so doesn't matter
          (this.copy(agentState = newAgentState, safeLocations = newSafeLocations), Forward)
        }
        case 1 => (this.copy(agentState = agentState.turnLeft), TurnLeft)
        case 2 => (this.copy(agentState = agentState.turnRight), TurnRight)
        case 3 => (this.copy(agentState = agentState.useArrow), Shoot)
      }
    }
  }
}

object BeelineAgent {

  def apply(
             gridWidth: Int,
             gridHeight: Int
           ): BeelineAgent = new BeelineAgent(
    gridWidth,
    gridHeight,
    new AgentState,
    Set.empty[Coords],
    List.empty[Action]
  )
}



