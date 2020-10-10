package wumpusWorld.agent

import wumpusWorld.environment.{Action, AgentState, Climb, Coords, Forward, Grab, Orientation, Percept, Shoot, TurnLeft, TurnRight}

case class BeelineBeliefState (
                        gridWidth: Int,
                        gridHeight: Int,
                        agentState: AgentState,
                        safeLocations: Set[Coords],
                        beelineActionList: List[Action]
                      ) {
  def nextBeliefState(lastAction: Action, percept: Percept): BeelineBeliefState = {

    val newAgentState = lastAction match {
      case Forward => agentState.forward(gridWidth, gridHeight)
      case TurnLeft => agentState.turnLeft
      case TurnRight => agentState.turnRight
      case Grab => agentState.copy(hasGold = agentState.hasGold || percept.glitter)
      case Shoot => agentState.copy(hasArrow = false) // not used for anything yet
      case Climb => agentState // game is over
    }

    val newSafeLocations: Set[Coords] = safeLocations + newAgentState.location

    BeelineBeliefState(
      gridWidth,
      gridHeight,
      newAgentState,
      newSafeLocations
    )
  }
}
