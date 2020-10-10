package wumpusWorld.agent

import wumpusWorld.environment.{Action, AgentState, Climb, Coords, Forward, Grab, Orientation, Percept, Shoot, TurnLeft, TurnRight}

case class ProbabilisticBeliefState (
                                gridWidth: Int,
                                gridHeight: Int,
                                agentState: AgentState,
                                breezeLocations: IndexedSeq[IndexedSeq[Option[Boolean]]],
                                pitLocations: IndexedSeq[IndexedSeq[Option[Boolean]]],
                                stenchLocations: IndexedSeq[IndexedSeq[Option[Boolean]]],
                                wumpusLocation: Option[Coords],
                                agentStateWhenUsedArrow: Option[AgentState],
                                screamPerceivedWhenUsedArrow: Boolean,
                                wumpusIsAlive: Boolean,
                                perceptHistory: List[Percept]
                              ) {
  def nextBeliefState(lastAction: Action, percept: Percept): BeliefState = {

    val newAgentState = lastAction match {
      case Forward => agentState.forward(gridWidth, gridHeight)
      case TurnLeft => agentState.turnLeft
      case TurnRight => agentState.turnRight
      case Grab => agentState.copy(hasGold = agentState.hasGold || percept.glitter)
      case Shoot => agentState.copy(hasArrow = false)
      case Climb => agentState // game is over
    }

    val newBreezeLocations: IndexedSeq[IndexedSeq[Option[Boolean]]] =
      breezeLocations
        .updated(newAgentState.location.x,
          breezeLocations(newAgentState.location.x)
            .updated(
              newAgentState.location.y,
              Some(percept.breeze)
            )
        )

    def inferPitLocations(breezeLocations: IndexedSeq[IndexedSeq[Option[Boolean]]]): IndexedSeq[IndexedSeq[Option[Boolean]]] = ???

    val newPitLocations = inferPitLocations(newBreezeLocations)

    val newStenchLocations =
      breezeLocations
        .updated(newAgentState.location.x,
          stenchLocations(newAgentState.location.x)
            .updated(
              newAgentState.location.y,
              Some(percept.stench)
            )
        )

    val newAgentStateWhenUsedArrow: Option[AgentState] = if (percept.scream) Some(newAgentState) else None

    val newScreamPerceivedWhenUsedArrow: Boolean = screamPerceivedWhenUsedArrow || percept.scream

    def inferWumpusLocation(
                             stenchLocations: IndexedSeq[IndexedSeq[Option[Boolean]]],
                             agentStateWhenUsedArrow: Option[AgentState],
                             screamPerceivedWhenUsedArrow: Boolean
                           ): Option[Coords] = ???

    val newWumpusLocation: Option[Coords] = inferWumpusLocation(
      stenchLocations,
      newAgentStateWhenUsedArrow,
      newScreamPerceivedWhenUsedArrow
    )

    val newWumpusIsAlive: Boolean = wumpusIsAlive && !percept.scream

    val newPerceptHistory: List[Percept] = percept :: perceptHistory

    BeelineBeliefState(
      gridWidth,
      gridHeight,
      newAgentState,
      newBreezeLocations,
      newPitLocations,
      newStenchLocations,
      newWumpusLocation,
      newAgentStateWhenUsedArrow,
      newScreamPerceivedWhenUsedArrow,
      newWumpusIsAlive,
      newPerceptHistory
    )

  }
}
