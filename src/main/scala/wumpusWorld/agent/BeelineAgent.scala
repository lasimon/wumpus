package wumpusWorld.agent

import scala.util.Random
import wumpusWorld.environment.{Action, AgentState, Climb, Coords, East, Forward, Grab, North, Orientation, Percept, Shoot, South, TurnLeft, TurnRight, West}

import scala.annotation.tailrec

case class BeelineAgent (
                            gridWidth: Int,
                            gridHeight: Int,
                            agentState: AgentState,
                            safeLocations: Set[Coords],
                            beelineActionList: List[Action]
                          ) extends Agent {

  private val randGen = new Random()

  private def growActionList(actionList: List[Action], nodesRemaining: List[Coords], agentState: AgentState): List[Action] = {
    def direction(from: Coords, to: Coords): Orientation = {
      if (from.x == to.x) {
        if (from.y < to.y) North else South
      } else {
        if (from.x < to.x) East else West
      }
    }

    def rotate(nodeDirection: Orientation, agentOrientation: Orientation): (Action, Orientation) = {
      (nodeDirection, agentOrientation) match {
        case (North, East) => (TurnLeft, North)
        case (South, East) => (TurnRight, South)
        case (West, East) => (TurnRight, South)
        case (North, West) => (TurnRight, North)
        case (South, West) => (TurnLeft, South)
        case (East, West) => (TurnRight, North)
        case (South, North) => (TurnRight, East)
        case (East, North) => (TurnRight, East)
        case (West, North) => (TurnLeft, West)
        case (North, South) => (TurnRight, West)
        case (East, South) => (TurnLeft, East)
        case (West, South) => (TurnRight, West)
      }
    }

    nodesRemaining match {
      case List(Coords(0,0)) => actionList
      case _ =>
        val directionToGo = direction(agentState.location, nodesRemaining(1))
        if (directionToGo == agentState.orientation) {
          growActionList(Forward :: actionList, nodesRemaining.tail, agentState.copy(location=nodesRemaining(1)))
        } else {
          val (action, newOrientation) = rotate(directionToGo, agentState.orientation)
          growActionList(action :: actionList, nodesRemaining, agentState.copy(orientation = newOrientation))
        }
    }
  }

  private def constructActionListFromPath(nodes: List[Coords], agentOrientation: Orientation): List[Action] = {
    growActionList(List.empty, nodes, agentState).reverse
    }

  @tailrec
  private def constructShortestPath(safeLocations: Set[Coords], frontier: Set[List[Coords]]): List[Coords] = {
    val newFrontier: Set[List[Coords]] = frontier.flatMap { path =>
      val safeAdjacentLocations: Set[Coords] = safeLocations.filter(location =>
        location.isAdjacentTo(path.head) && !path.contains(location)
      )
      safeAdjacentLocations.map(_ :: path)
      }

    newFrontier.find(path => path.head == Coords(0,0)) match {
      case Some(path) => path
      case None => constructShortestPath(safeLocations, newFrontier)
    }
  }

  private def findShortestPathHome(coords: Coords, safeLocations: Set[Coords]): List[Coords] = {
    constructShortestPath(safeLocations, Set(List(coords))).reverse
  }

  private def constructBeelinePlan(fromLocation: Coords): List[Action] = {
    val shortestPath: List[Coords] = findShortestPathHome(fromLocation, safeLocations)
    constructActionListFromPath(shortestPath, agentState.orientation)
  }

  def nextAction(percept: Percept): (BeelineAgent, Action)  = {
    if (agentState.hasGold) { // we're in the endgame
      if (agentState.location == Coords(0,0)) { // we have a winner
        (this, Climb)
      } else { // we have the gold but aren't home yet. Construct a plan if we don't have one.
        val beelinePlan: List[Action] = if (beelineActionList.isEmpty) constructBeelinePlan(agentState.location) else beelineActionList
        val beelineAction: Action = beelinePlan.head // take the action at the head of the list of remaining actions
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
      searchForGold(percept)
    }
  }

  def searchForGold(percept: Percept): (BeelineAgent, Action) = {
    randGen.nextInt(4) match {
      case 0 =>
        val newAgentState = agentState.forward(gridWidth, gridHeight)
        val newSafeLocations: Set[Coords] = safeLocations + newAgentState.location // move may not actually be safe, but if not agent will be dead so doesn't matter
        (this.copy(agentState = newAgentState, safeLocations = newSafeLocations), Forward)
      case 1 => (this.copy(agentState = agentState.turnLeft), TurnLeft)
      case 2 => (this.copy(agentState = agentState.turnRight), TurnRight)
      case 3 => (this.copy(agentState = agentState.useArrow), Shoot)
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
    Set(Coords(0, 0)),
    List.empty[Action]
  )
}



