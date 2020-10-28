package wumpusWorld.agent

import com.cra.figaro.algorithm.factored.{ProbQueryVariableElimination, VariableElimination}
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.discrete.{AtomicUniform, Uniform}
import com.cra.figaro.language.{AtomicFlip, AtomicSelect, Chain, Constant, Element, Flip, Select}
import com.cra.figaro.library.compound.If
import wumpusWorld.Bayes.Probability
import wumpusWorld.environment.{Action, AgentState, Climb, Coords, East, Forward, Grab, North, Orientation, Percept, Shoot, South, TurnLeft, TurnRight, West}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Random

case class ProbAgent(
                      gridWidth: Int,
                      gridHeight: Int,
                      pitProb: Probability,
                      agentState: AgentState = new AgentState,
                      beelineActionList: List[Action] = List(), // the sequence of Actions remaining to be done to get back to the start location from the current location/orientation
                      breezeLocations: Set[Coords] = Set(), // locations a breeze has been sensed
                      stenchLocations: Set[Coords] = Set(), // locations a stench has been previously sensed
                      visitedLocations: Set[Coords] = Set(), // locations the agent has previously visited
                      heardScream: Boolean = false, // true if a scream has been sensed at any previous timestep
                      inferredPitProbs: IndexedSeq[IndexedSeq[Double]] = IndexedSeq(IndexedSeq.empty), // pit probabilities carried forward to avoid unnecessarily recomputing them
                      inferredWumpusProbs: IndexedSeq[IndexedSeq[Double]] = IndexedSeq(IndexedSeq.empty) // same for wumpus probabilities; these are both for efficiency only
                    ) extends Agent {

  private val randGen = new Random()

  // Beeline methods

  // Recursive function to add an Action (TurnLeft, TurnRight, Forward) onto the head of a growing list of beeline actions
  // given the actionList built so far, the list of cells that we need to pass through but have yet to construct actions for, and the state of the agent
  // at the point in the journey home we're adding an action for
  private def growActionList(actionList: List[Action], nodesRemaining: List[Coords], agentState: AgentState): List[Action] = {
    // Return the orientation the agent needs to be in to move between adjacent cells from and to
    // (Note Scala returns the last thing evaluated in a function (i.e. there is no return keyword) and if is like a ternary (.. ? .. : ..) in other languages)
    def direction(from: Coords, to: Coords): Orientation = {
      if (from.x == to.x) {
        if (from.y < to.y) North else South
      } else {
        if (from.x < to.x) East else West
      }
    }

    // Return the correct turn action and resulting orientation given the agent's current orientation and the direction needed to move to the next node
    // (Scala match ... case is similar to switch in other languages)
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

    // Here's where the action is chosen and added using the two helper functions above
    // NOTE: This can be easily generalized to produce an action list to move to any target by replacing the (0, 0) below with a function parameter (which would be useful for the searchForGold function)
    nodesRemaining match { // for all the remaining cells we need to pass through
      case List(Coords(0, 0)) => actionList // if the only one is the start position, we're done the beeline plan, so return the complete actionList
      case _ => // otherwise
        val directionToGo = direction(agentState.location, nodesRemaining(1)) // what direction does the agent need to face to go to the next cell
        if (directionToGo == agentState.orientation) { // if already facing that direction
          growActionList(Forward :: actionList, nodesRemaining.tail, agentState.copy(location = nodesRemaining(1))) // put a Forward on the head of the action list then recurse with the rest of the list
        } else {
          val (action, newOrientation) = rotate(directionToGo, agentState.orientation) // determine the correct turn to move towards alignment for the next Forward
          growActionList(action :: actionList, nodesRemaining, agentState.copy(orientation = newOrientation)) // put the turn at the head of the action list then recurse with the entire list
        }
    }
  }

  // Construct a beeline action list for the given sequence of cells
  private def constructActionListFromPath(nodes: List[Coords]): List[Action] = {
    growActionList(List.empty, nodes, agentState).reverse
  }

  @tailrec // This annotation says that I expect this function to be tail-recursive which the compiler will automatically convert from recursion to iteration (not important)
  private def constructShortestPath(safeLocations: Set[Coords], frontier: Set[List[Coords]]): List[Coords] = { // recursively extend the frontier using breadth-first search given safe locations to use and the frontier
    val newFrontier: Set[List[Coords]] = frontier.flatMap { path => // for each path in the frontier
      val safeAdjacentLocations: Set[Coords] = safeLocations.filter(location => // what are the safe location
        location.isAdjacentTo(path.head) && !path.contains(location) // that are adjacent to the frontier but not already in the path (note: this could probably be made tighter by considering only nodes not on any frontier path)
      )
      safeAdjacentLocations.map(_ :: path) // put the new breadth-first nodes onto the FRONT of all of the frontier paths
    }

    newFrontier.find(path => path.head == Coords(0, 0)) match { // see if we have a path that now extends to (0, 0)
      case Some(path) => path // if so, we're done and can return that path
      case None => constructShortestPath(safeLocations, newFrontier) // if not, recurse with the new frontier
    }
  }

  // Find a shortest path to (0, 0) from the given coords
  private def findShortestPathHome(coords: Coords, safeLocations: Set[Coords]): List[Coords] = {
    constructShortestPath(safeLocations, Set(List(coords))).reverse // construct the path from coords to (0, 0) which constructShortestPath returns in reverse order so we need to reverse it to the correct order
  }

  // Construct the beeline plan (list of actions) by finding the
  private def constructBeelinePlan(fromLocation: Coords, safeLocations: Set[Coords]): List[Action] = {
    val shortestPath: List[Coords] = findShortestPathHome(fromLocation, safeLocations)
    constructActionListFromPath(shortestPath)
  }

  // Probability methods and values

  // Given a cell coordinates and a grid array return what is in the grid array at those coords
  private def neighbours[E](coords: Coords, array: Array[Array[E]]): List[E] = {
    val adjacentCells: List[Coords] = coords.adjacentCells(gridWidth, gridHeight)
    adjacentCells.map(cell => array(cell.x)(cell.y))
  }

  // Create a grid with a discrete Bernoulli distribution for each cell
  // (0, 0) is always false
  val pitProbs: Array[Array[AtomicFlip]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Flip(if (i == 0 && j == 0) 0.0 else pitProb.value)
  )

  // Create a discrete distribution over the (gridWidth * gridHeight - 1) locations the Wumpus could be
  // Note that Figaro specifies these as for example Select(List(0.3 -> 'red', 0.7 -> 'green')) whereas Pomegranate would do it as {'red': 0.3, 'green': 0.7}
  // The Scala _* operator is similar to the spread operator (...) in JavaScript; it converts the list of grid coords to a comma-separated list of arguments to the Select
  val wumpusLocationDist: AtomicSelect[Coords] = {
    val gridMap: immutable.IndexedSeq[(Double, Coords)] = for (i <- Range(0, gridWidth); j <- Range(0, gridHeight))
      yield (1.0 / (gridWidth * gridHeight - 1) -> Coords(i, j))
    Select((List(0.0 -> Coords(0, 0)) ++ gridMap.toList.tail): _*)
  }

  // Make a conditional probability distribution (CPD) for each location in the grid which is true if the wumpusLocationDist selects its coordinates and false otherwise
  // Chain ties it as a dependence of wumpusLocationDist, like adding an edge in Pomegranate
  val wumpusProbs: Array[Array[Chain[Coords, Boolean]]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Chain(wumpusLocationDist, (wumpusIsAt: Coords) => if (wumpusIsAt == Coords(i,j)) Constant(true) else Constant(false))
  )

  // Make a CPD for each cell that is dependent on its 2-4 neighbour cells
  // I had to use a Figaro Container and the exists method on Container to check whether any of each cell's neighbours (its parents in the graph) are true
  // .exists(x => x) means: if the condition in the parentheses is true of any of the parents; x => x will just be true or false if the parent is true or false
  val breezeCPDs: Array[Array[Element[Boolean]]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Container(neighbours(Coords(i, j), pitProbs): _*).exists(x => x)
  )

  // Same thing with stenches
  val stenchCPDs: Array[Array[Element[Boolean]]] = Array.tabulate(gridWidth, gridHeight)((i, j) =>
    Container(neighbours(Coords(i, j), wumpusProbs): _*).exists(x => x)
  )

  // Methods for conducting outwards search

  // NOTE: Do not use this for your solution!  This is an almost-naive search.  You need to do a smarter search.  This agent will occasionally go in circles and crash the app!
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

  def nextAction(percept: Percept): (ProbAgent, Action) = {

    // Update percepts tracking: add new percepts to our sets of historically perceived ones

    val visitingNewLocation: Boolean = !visitedLocations.contains(agentState.location)
    val newVisitedLocations: Set[Coords] = visitedLocations + agentState.location
    val newBreezeLocations: Set[Coords] = if (percept.breeze) breezeLocations + agentState.location else breezeLocations
    val newStenchLocations: Set[Coords] = if (percept.stench) stenchLocations + agentState.location else stenchLocations
    val newHeardScream: Boolean = heardScream || percept.scream

    // Set prob model observations: for each location in the grid where we have observed a breeze or stench, add an observation of true to the appropriate PGM
    // For locations we have visited but did not sense a breeze or stench update the models with an observation of false

    newBreezeLocations.foreach(location => breezeCPDs(location.x)(location.y).observe(true))
    newVisitedLocations.filter(!newBreezeLocations.contains(_))
      .foreach(location => breezeCPDs(location.x)(location.y).observe(false))
    newStenchLocations.foreach(location => stenchCPDs(location.x)(location.y).observe(true))
    newVisitedLocations.filter(!newStenchLocations.contains(_))
      .foreach(location => stenchCPDs(location.x)(location.y).observe(false))
    newVisitedLocations.foreach(location => pitProbs(location.x)(location.y).observe(false))
    newVisitedLocations.foreach(location => wumpusProbs(location.x)(location.y).observe(false)) // this will be wrong if wumpus is dead but we don't care at that point

    // Methods to compute and allow lookup of prob model values

    // Performance optimization: We will only need to do inference on pit locations if we just moved to a new location and aren't beelining
    val updatePitProbs: Boolean = visitingNewLocation && !percept.glitter

    // Performance optimization: Attempt to only run inference for 100 ms. but doesn't seem to work
    val pitAlgo: ProbQueryVariableElimination = VariableElimination(pitProbs.flatMap(_.toList): _*)
    if (updatePitProbs) {
      pitAlgo.start
      Thread.sleep(100)
      pitAlgo.stop
    }

    // Return the probability of there being a pit at the given coords
    def probOfPit(coords: Coords): Double = pitAlgo.probability(pitProbs(coords.x)(coords.y), true)

    // Performance optimization: We will only need to do inference on Wumpus locations if we just moved to a new location and aren't beelining and didn't just hear a scream
    val updateWumpusProbs: Boolean = visitingNewLocation && !percept.scream && !percept.glitter

    // Performance optimization: Attempt to only run inference for 100 ms. but doesn't seem to work
    val wumpusAlgo: ProbQueryVariableElimination = VariableElimination(wumpusLocationDist)
    if (updateWumpusProbs) {
      wumpusAlgo.start
      Thread.sleep(100);
      wumpusAlgo.stop
    }

    // Return the probability of there being a Wumpus at the given coords; always return 0 if a scream has been heard regardless of what the PGM says
    def probOfLiveWumpus(coords: Coords): Double = if (newHeardScream) 0.0 else {
     wumpusAlgo.probability(wumpusLocationDist, coords)
    }

    // A grid with its coordinates at each location.  This is a convenience for DRY code.
    val allLocations: IndexedSeq[IndexedSeq[Coords]] = for (i <- Range(0, gridWidth)) yield for (j <- Range(0, gridHeight)) yield Coords(i, j)

    // Compute the new pit probabilities
    val newInferredPitProbs: IndexedSeq[IndexedSeq[Double]] =
      if (updatePitProbs) { // this is to avoid recomputing unless necessary
        val ps: IndexedSeq[IndexedSeq[Double]] = allLocations.map(row => row.map(coords => probOfPit(coords)))
        pitAlgo.kill
        ps
    } else inferredPitProbs

    // Print the pit probabilities
    println("PitProbs:")
    for (j <- Range(gridHeight - 1, -1, -1)) {
      for (i <- Range(0, gridWidth)) print(f"${newInferredPitProbs(i)(j)}%2.2f ")
      println()
    }

    // Compute the Wumpus probabilities
    val newInferredWumpusProbs: IndexedSeq[IndexedSeq[Double]] = if (updateWumpusProbs) { // this is to avoid recomputing unless necessary
      val ps: IndexedSeq[IndexedSeq[Double]] = allLocations.map(row => row.map(coords => probOfLiveWumpus(coords)))
      wumpusAlgo.kill
      ps
    } else inferredWumpusProbs

    // Print the Wumpus probabilities
    println("WumpusProbs:")
    for (j <- Range(gridHeight - 1, -1, -1)) {
      for (i <- Range(0, gridWidth)) print(f"${newInferredWumpusProbs(i)(j)}%2.2f ")
      println()
    }

    // Methods for choosing next move or heading home

    // Return a list of locations that have a probability of a pit or Wumpus less than the given tolerance (acceptable risk)
    def safeLocations(tolerance: Double): Set[Coords] = {
     allLocations.flatMap(row => row.filter(location => newInferredPitProbs(location.x)(location.y) < tolerance && newInferredWumpusProbs(location.x)(location.y) < tolerance)).toSet ++ visitedLocations
    }

    // Should be called when the agent has the gold
    // Return the next action for the agent to take while beelining along with the new state the agent will be in and the remaining actions not yet taken
    // Note that the parenthesis around the lines starting with agentState.applyMoveAction and in the function signature mean that we're returning a tuple with 3 elements
    def beeline: (AgentState, List[Action], Action) = {
      println("beelining")
      val beelinePlan: List[Action] = if (beelineActionList.isEmpty) constructBeelinePlan(agentState.location, safeLocations(0.001)) else beelineActionList // if we have a previously-constructed action plan use it, otherwise create one
      println("action list:", beelinePlan)
      val beelineAction: Action = beelinePlan.head // take the action at the head of the list of remaining actions
      (
        agentState.applyMoveAction(beelineAction, gridWidth, gridHeight),
        beelinePlan.tail, // set the beelineActionList to the actions remaining to be done
        beelineAction
      )
    }

    // Print the safe locations for beelining and for exploration
    // 0.01 was chosen to allow for the possibility of using approximate inference which may not be exactly 0
    // 0.4 was chosen as a seemingly-reasonable tradeoff between risking the agent's life in return for a better long-term result (not clear if this is anywhere near optimal)
    println(s"Safe Locations(0.01): ${safeLocations(0.01)}")
    println(s"Safe Locations(0.4): ${safeLocations(0.4)}")

    // Choose the action (and resulting new agent state and, if beelining, the remaining beeline actions to pass forward to the next time)
    val (newAgentState, newBeelineActionList, action) =
      if (agentState.hasGold && agentState.location == Coords(0, 0)) // if we have the gold and we're at (0,0) we're done, so Climb
        (agentState, List(), Climb)
      else if (agentState.hasGold) { // if we're not at (0,0) but have the gold we should be beelining
        beeline
      }
     else if (percept.glitter && !agentState.hasGold) { // if we don't have the gold but we're in the room with it we should Grab it
      (agentState.copy(hasGold = true), List(), Grab)
    } else if (percept.stench && agentState.hasArrow) { // if we perceive a stench, let's shoot indisciminately NOTE: Your code needs to be smarter than this
      (agentState.useArrow, beelineActionList, Shoot)
    } else if (!agentState.location.adjacentCells(gridWidth, gridHeight).exists(location => safeLocations(0.4).contains(location))) { // if no safe next locations available, abort mission
        if (agentState.location == Coords(0, 0))
          (agentState, List(), Climb)
        else
          beeline
    } else { // we don't have the gold and aren't in the gold room so explore
      val (newAgentState, action) = searchForGold(percept, safeLocations(0.40))
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
