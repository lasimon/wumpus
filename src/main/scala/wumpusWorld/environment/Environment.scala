package wumpusWorld.environment

import wumpusWorld.Bayes.Probability

import scala.util.Random

sealed trait Orientation {
  def turnLeft: Orientation

  def turnRight: Orientation
}

case object North extends Orientation {
  def turnLeft: Orientation = West

  def turnRight: Orientation = East
}

case object South extends Orientation {
  def turnLeft: Orientation = East

  def turnRight: Orientation = West
}

case object East extends Orientation {
  def turnLeft: Orientation = North

  def turnRight: Orientation = South
}

case object West extends Orientation {
  def turnLeft: Orientation = South

  def turnRight: Orientation = North
}

sealed trait Action

case object Forward extends Action

case object TurnLeft extends Action

case object TurnRight extends Action

case object Shoot extends Action

case object Grab extends Action

case object Climb extends Action

case class Coords(x: Int, y: Int) {
   def isAdjacentTo(coords: Coords): Boolean =
     (x == coords.x && Math.abs(coords.y - y) == 1) || (y == coords.y && Math.abs(coords.x - x) == 1)

  def adjacentCells(gridWidth: Int, gridHeight: Int): List[Coords] = {
    val toLeft: List[Coords] = if (x > 0) List(Coords(x - 1, y)) else Nil

    val toRight: List[Coords] = if (x < gridWidth - 1) List(Coords(x + 1, y)) else Nil

    val below: List[Coords] = if (y > 0) List(Coords(x, y - 1)) else Nil

    val above: List[Coords] = if (y < gridHeight - 1) List(Coords(x, y + 1)) else Nil

    toLeft ::: toRight ::: below ::: above
  }
}

case class Percept(stench: Boolean, breeze: Boolean, glitter: Boolean, bump: Boolean, scream: Boolean, isTerminated: Boolean, reward: Double) {
  def show: String = s"stench:$stench breeze:$breeze glitter:$glitter bump:$bump scream:$scream isTerminated:$isTerminated reward:$reward"
}

final case class Environment private(
                                      gridWidth: Int,
                                      gridHeight: Int,
                                      pitProb: Probability,
                                      allowClimbWithoutGold: Boolean,
                                      agent: AgentState,
                                      pitLocations: List[Coords],
                                      terminated: Boolean,
                                      wumpusLocation: Coords,
                                      wumpusAlive: Boolean,
                                      goldLocation: Coords
                                    ) {
  private def isPitAt(coords: Coords): Boolean = pitLocations.contains(coords)

  private def isWumpusAt(coords: Coords): Boolean = wumpusLocation == coords

  private def isAgentAt(coords: Coords): Boolean = coords == agent.location

  private def isGlitter: Boolean = goldLocation == agent.location

  private def isGoldAt(coords: Coords): Boolean = coords == goldLocation

  private def killAttemptSuccessful: Boolean = {
    def wumpusInLineOfFire: Boolean = {
      agent.orientation match {
        case West => agent.location.x > wumpusLocation.x && agent.location.y == wumpusLocation.y
        case East => agent.location.x < wumpusLocation.x && agent.location.y == wumpusLocation.y
        case South => agent.location.x == wumpusLocation.x && agent.location.y > wumpusLocation.y
        case North => agent.location.x == wumpusLocation.x && agent.location.y < wumpusLocation.y
      }
    }

    agent.hasArrow && wumpusAlive && wumpusInLineOfFire
  }

  private def isPitAdjacent(coords: Coords): Boolean = {
    coords.adjacentCells(gridWidth, gridHeight).exists(cell => pitLocations.contains(cell))
  }

  private def isWumpusAdjacent(coords: Coords): Boolean = {
    coords.adjacentCells(gridWidth, gridHeight).exists(cell => isWumpusAt(cell))
  }

  private def isBreeze: Boolean = isPitAdjacent(agent.location)

  private def isStench: Boolean = isWumpusAdjacent(agent.location) || isWumpusAt(agent.location)
  
  def applyAction(action: Action): (Environment, Percept) = {

    if (terminated)
      (
        this,
        Percept(false, false, false, false, false, true, 0)
      )
    else {
      action match {
        case Forward =>
          val movedAgent = agent.forward(gridWidth, gridHeight)
          val death = (isWumpusAt(movedAgent.location) && wumpusAlive) || isPitAt(movedAgent.location)
          val newAgent = movedAgent.copy(isAlive = !death)
          val newEnv = new Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold, newAgent, pitLocations, death, wumpusLocation, wumpusAlive,  if (agent.hasGold) newAgent.location else goldLocation)
          (
            newEnv,
            Percept(newEnv.isStench, newEnv.isBreeze, newEnv.isGlitter, newAgent.location == agent.location, false, !newAgent.isAlive, if (newAgent.isAlive) -1 else -1001)
          )
        case TurnLeft =>
          (
            new Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold, agent.turnLeft, pitLocations, terminated, wumpusLocation, wumpusAlive, goldLocation),
            Percept(isStench, isBreeze, isGlitter,false, false,  false, -1)
          )
        case TurnRight =>
          (
            new Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold, agent.turnRight, pitLocations, terminated, wumpusLocation, wumpusAlive, goldLocation),
            Percept(isStench, isBreeze, isGlitter,false, false,  false, -1)
          )
        case Grab =>
          val newAgent = agent.copy(hasGold = isGlitter)
          (
            new Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold, newAgent, pitLocations, terminated, wumpusLocation, wumpusAlive, if (newAgent.hasGold) agent.location else goldLocation),
            Percept(isStench, isBreeze, isGlitter,false, false,  false, -1)
          )
        case Climb =>
          val inStartLocation = agent.location == Coords(0, 0)
          val success = agent.hasGold && inStartLocation
          val isTerminated = success || (allowClimbWithoutGold && inStartLocation)
          (
            new Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold, agent, pitLocations, isTerminated, wumpusLocation, wumpusAlive, goldLocation),
            Percept(false, false, isGlitter, false, false, isTerminated, if (success) 999 else -1)
          )
        case Shoot =>
          val hadArrow = agent.hasArrow
          val wumpusKilled = killAttemptSuccessful
          (
            new Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold, agent.copy(hasArrow = false), pitLocations, terminated, wumpusLocation, wumpusAlive && !wumpusKilled, goldLocation),
            Percept(isStench, isBreeze, isGlitter, false, wumpusKilled, false, if (hadArrow) -11 else -1)
          )
      }
    }
  }

  def visualize: String = {
    val wumpusSymbol = if (wumpusAlive) "W" else "w"
    val rows = for {
      y <- gridHeight - 1 to 0 by -1
      cells = for {
        x <- 0 until gridWidth
        c = s"${if (isAgentAt(Coords(x, y))) "A" else " "}${if (isPitAt(Coords(x, y))) "P" else " "}${if (isGoldAt(Coords(x, y))) "G" else " "}${if (isWumpusAt(Coords(x, y))) wumpusSymbol else " "}"
      } yield c
      row = cells.mkString("|")
    } yield row
    rows.mkString("\n")
  }
}

object Environment {
  private val randGen = new Random()

  def apply(
             gridWidth: Int,
             gridHeight: Int,
             pitProb: Probability,
             allowClimbWithoutGold: Boolean
           ): (Environment, Percept) = {

    def randomLocationExceptOrigin: Coords = {
      val x = randGen.nextInt(gridWidth)
      val y = randGen.nextInt(gridHeight)
      if (x == 0 && y == 0) // try again if (0, 0)
        randomLocationExceptOrigin
      else
        Coords(x, y)
    }

    val pitLocations: List[Coords] = {
      val cellIndexes =
        for (x <- 0 until gridWidth; y <- 0 until gridHeight)
          yield Coords(x, y)
      cellIndexes.tail.filter(_ => randGen.nextFloat < pitProb.value) // tail removes (0, 0)
    }.toList

    val env = new Environment(
      gridWidth,
      gridHeight,
      pitProb,
      allowClimbWithoutGold,
      AgentState(),
      pitLocations,
      false,
      randomLocationExceptOrigin,
      true,
      randomLocationExceptOrigin
    )

    (
      env,
      Percept(env.isStench, env.isBreeze, false,false, false,  false, 0.0)
    )
  }
}


