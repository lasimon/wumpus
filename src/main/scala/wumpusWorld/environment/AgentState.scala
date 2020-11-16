package wumpusWorld.environment

import scala.math.{max, min}

case class AgentState(
                  location: Coords = Coords(0, 0),
                  orientation: Orientation = East,
                  hasGold: Boolean = false,
                  hasArrow: Boolean = true,
                  isAlive: Boolean = true
                ) {
  def turnLeft: AgentState = this.copy(orientation = orientation.turnLeft)

  def turnRight: AgentState = this.copy(orientation = orientation.turnRight)

  def forward(gridWidth: Int, gridHeight: Int): AgentState = {
    val newAgentLocation: Coords = orientation match {
      case West => Coords(max(0, location.x - 1), location.y)
      case East => Coords(min(gridWidth - 1, location.x + 1), location.y)
      case South => Coords(location.x, max(0, location.y - 1))
      case North => Coords(location.x, min(gridHeight - 1, location.y + 1))
    }
    this.copy(location = newAgentLocation)
  }

  def useArrow: AgentState = this.copy(hasArrow = false)

  def applyMoveAction(action: Action, gridWidth: Int, gridHeight: Int): AgentState = {
        action match {
          case TurnLeft => turnLeft
          case TurnRight => turnRight
          case Forward => forward(gridWidth, gridHeight)
          case _ => this
        }
  }

  def applyAction(actionOption: Option[Action], gridWidth: Int, gridHeight: Int): AgentState = actionOption match {
    case Some(action) => if (action == Shoot) useArrow else this.applyMoveAction(action: Action, gridWidth: Int, gridHeight: Int)
    case None => this
  }

  def show: String = s"location: $location orientation: $orientation hasGold: $hasGold hasArrow: $hasArrow isAlive; $isAlive"
}
