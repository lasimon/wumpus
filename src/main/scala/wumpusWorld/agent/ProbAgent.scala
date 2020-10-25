package wumpusWorld.agent

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.discrete.{AtomicUniform, Uniform}
import com.cra.figaro.language.{AtomicFlip, Chain, Constant, Element, Flip, Select}
import com.cra.figaro.library.compound.If
import wumpusWorld.Bayes.Probability
import wumpusWorld.environment.{Action, AgentState, Climb, Coords, East, Forward, Grab, North, Orientation, Percept, Shoot, South, TurnLeft, TurnRight, West}

import scala.annotation.tailrec
import scala.util.Random

case class ProbAgent(
                      gridWidth: Int,
                      gridHeight: Int,
                      pitProb: Probability,
                      agentState: AgentState = new AgentState,
                      beelineActionList: List[Action] = List(),
                      breezeLocations: Set[Coords] = Set(),
                      stenchLocations: Set[Coords] = Set(),
                      visitedLocations: Set[Coords] = Set(),
                      heardScream: Boolean = false,
                      inferredPitProbs: IndexedSeq[IndexedSeq[Double]] = IndexedSeq(IndexedSeq.empty),
                      inferredWumpusProbs: IndexedSeq[IndexedSeq[Double]] = IndexedSeq(IndexedSeq.empty)
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
      case List(Coords(0, 0)) => actionList
      case _ =>
        val directionToGo = direction(agentState.location, nodesRemaining(1))
        if (directionToGo == agentState.orientation) {
          growActionList(Forward :: actionList, nodesRemaining.tail, agentState.copy(location = nodesRemaining(1)))
        } else {
          val (action, newOrientation) = rotate(directionToGo, agentState.orientation)
          growActionList(action :: actionList, nodesRemaining, agentState.copy(orientation = newOrientation))
        }
    }
  }

  // Beeline methods

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

    newFrontier.find(path => path.head == Coords(0, 0)) match {
      case Some(path) => path
      case None => constructShortestPath(safeLocations, newFrontier)
    }
  }

  private def findShortestPathHome(coords: Coords, safeLocations: Set[Coords]): List[Coords] = {
    constructShortestPath(safeLocations, Set(List(coords))).reverse
  }

  private def constructBeelinePlan(fromLocation: Coords, safeLocations: Set[Coords]): List[Action] = {
    val shortestPath: List[Coords] = findShortestPathHome(fromLocation, safeLocations)
    constructActionListFromPath(shortestPath, agentState.orientation)
  }

  // Probability methods and values

  private def neighbours[E](coords: Coords, array: Array[Array[E]]): List[E] = {
    val adjacentCells: List[Coords] = coords.adjacentCells(gridWidth, gridHeight)
    adjacentCells.map(cell => array(cell.x)(cell.y))
  }

  val pitProbs: Array[Array[AtomicFlip]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Flip(if (i == 0 && j == 0) 0.0 else pitProb.value)
  )

  val wumpusLocationDist = {
    val gridMap = for (i <- Range(0, gridWidth); j <- Range(0, gridHeight))
      yield (1.0 / (gridWidth * gridHeight - 1) -> Coords(i, j))
    Select((List(0.0 -> Coords(0, 0)) ++ gridMap.toList.tail): _*)
  }

  val wumpusProbs = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Chain(wumpusLocationDist, (v: Coords) => if (v == Coords(i,j)) Constant(true) else Constant(false))
  )

  val breezeCPDs: Array[Array[Element[Boolean]]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Container(neighbours(Coords(i, j), pitProbs): _*).exists(x => x)
  )

  val stenchCPDs: Array[Array[Element[Boolean]]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Container(neighbours(Coords(i, j), wumpusProbs): _*).exists(x => x)
  )

  // Methods for conducting outwards search

  def searchForGold(percept: Percept, safeLocations: Set[Coords]): (AgentState, Action) = {
    val forwardLocation = agentState.forward(gridWidth, gridHeight).location
    if (percept.bump || forwardLocation == agentState.location || !safeLocations.contains(forwardLocation))
      (agentState.turnRight, TurnRight)
    else randGen.nextInt(3) match {
      case 0 => (agentState.forward(gridWidth, gridHeight), Forward)
      case 1 => (agentState.forward(gridWidth, gridHeight), Forward)
      case 2 => (agentState.turnRight, TurnRight)
    }
  }

  // nextAction methods

  def nextAction(percept: Percept) = {

    // Update percepts tracking

    val visitingNewLocation: Boolean = !visitedLocations.contains(agentState.location)
    val newVisitedLocations: Set[Coords] = visitedLocations + agentState.location
    val newBreezeLocations: Set[Coords] = if (percept.breeze) breezeLocations + agentState.location else breezeLocations
    val newStenchLocations: Set[Coords] = if (percept.stench) stenchLocations + agentState.location else stenchLocations
    val newHeardScream: Boolean = heardScream || percept.scream

    // Set prob model observations

    newBreezeLocations.foreach(location => breezeCPDs(location.x)(location.y).observe(true))
    newVisitedLocations.filter(!newBreezeLocations.contains(_))
      .foreach(location => breezeCPDs(location.x)(location.y).observe(false))
    newStenchLocations.foreach(location => stenchCPDs(location.x)(location.y).observe(true))
    newVisitedLocations.filter(!newStenchLocations.contains(_))
      .foreach(location => stenchCPDs(location.x)(location.y).observe(false))
    newVisitedLocations.foreach(location => pitProbs(location.x)(location.y).observe(false))
    newVisitedLocations.foreach(location => wumpusProbs(location.x)(location.y).observe(false)) // this will be wrong if wumpus is dead but we don't care at that point

    // Methods to compute and allow lookup of prob model values

    val updatePitProbs = visitingNewLocation && !percept.glitter

    val pitAlgo = VariableElimination(pitProbs.flatMap(_.toList): _*)
    if (updatePitProbs) {
      pitAlgo.start
      Thread.sleep(100)
      pitAlgo.stop
    }

    def probOfPit(coords: Coords): Double = pitAlgo.probability(pitProbs(coords.x)(coords.y), true)

    val updateWumpusProbs = visitingNewLocation && !heardScream && !percept.glitter

    val wumpusAlgo = VariableElimination(wumpusLocationDist)
    if (updateWumpusProbs) {
      wumpusAlgo.start
      Thread.sleep(100);
      wumpusAlgo.stop
    }

    def probOfLiveWumpus(coords: Coords): Double = if (newHeardScream) 0.0 else {
     wumpusAlgo.probability(wumpusLocationDist, coords)
    }

    val allLocations: IndexedSeq[IndexedSeq[Coords]] = for (i <- Range(0, gridWidth)) yield for (j <- Range(0, gridHeight)) yield Coords(i, j)

    val newInferredPitProbs: IndexedSeq[IndexedSeq[Double]] =
      if (updatePitProbs) { // this is to avoid recomputing unless necessary
        val ps = allLocations.map(row => row.map(coords => probOfPit(coords)))
        pitAlgo.kill
        ps
    } else inferredPitProbs

    println("PitProbs:")
    for (j <- Range(gridHeight - 1, -1, -1)) {
      for (i <- Range(0, gridWidth)) print(f"${newInferredPitProbs(i)(j)}%2.2f ")
      println()
    }

    val newInferredWumpusProbs: IndexedSeq[IndexedSeq[Double]] = if (updateWumpusProbs) { // this is to avoid recomputing unless necessary
      val ps = allLocations.map(row => row.map(coords => probOfLiveWumpus(coords)))
      wumpusAlgo.kill
      ps
    } else inferredWumpusProbs


    println("WumpusProbs:")
    for (j <- Range(gridHeight - 1, -1, -1)) {
      for (i <- Range(0, gridWidth)) print(f"${newInferredWumpusProbs(i)(j)}%2.2f ")
      println()
    }

    // Methods for choosing next move or heading home

    def safeLocations(tolerance: Double): Set[Coords] = {
     allLocations.flatMap(row => row.filter(location => newInferredPitProbs(location.x)(location.y) < tolerance && newInferredWumpusProbs(location.x)(location.y) < tolerance)).toSet ++ visitedLocations
    }

    def beeline = {
      println("beelining")
      val beelinePlan: List[Action] = if (beelineActionList.isEmpty) constructBeelinePlan(agentState.location, safeLocations(0.001)) else beelineActionList
      println("action list:", beelinePlan)
      val beelineAction: Action = beelinePlan.head // take the action at the head of the list of remaining actions
      (
        agentState.applyMoveAction(beelineAction, gridWidth, gridHeight),
        beelinePlan.tail, // set the beelineActionList to the actions remaining to be done
        beelineAction
      )
    }

    println(s"Safe Locations(0.01): ${safeLocations(0.01)}")
    println(s"Safe Locations(0.4): ${safeLocations(0.4)}")

    val (newAgentState, newBeelineActionList, action) =
      if (agentState.hasGold && agentState.location == Coords(0, 0)) {
        (agentState, List(), Climb)
      } else if (agentState.hasGold) {
        beeline
      }
     else if (percept.glitter && !agentState.hasGold) { // we don't have the gold but we're in the room with it
      (agentState.copy(hasGold = true), List(), Grab)
    } else if (percept.stench && agentState.hasArrow) {
      (agentState.useArrow, beelineActionList, Shoot)
    } else if (!agentState.location.adjacentCells(gridWidth, gridHeight).exists(location => safeLocations(0.4).contains(location))) { // if no safe next location abort
        if (agentState.location == Coords(0, 0))
          (agentState, List(), Climb)
        else
          beeline
    } else { // we don't have the gold and aren't in the gold room so explore
      val (newAgentState, action) = searchForGold(percept, safeLocations(0.25))
      (newAgentState, List(), action)
    }

    (
      this.copy(
        breezeLocations = newBreezeLocations,
        stenchLocations = newStenchLocations,
        agentState = newAgentState,
        visitedLocations = newVisitedLocations,
        beelineActionList = newBeelineActionList,
        heardScream = newHeardScream,
        inferredPitProbs = newInferredPitProbs,
        inferredWumpusProbs = newInferredWumpusProbs
      ),
      action
    )
  }
}
