package wumpusWorld.environment

import scala.math.{max, min}

case class Agent(
  location: Coords = Coords(0, 0),
  orientation: Orientation = East,
  hasGold: Boolean = false,
  hasArrow: Boolean = true,
  isAlive: Boolean = true
) {
  def turnLeft: Agent = this.copy(orientation = orientation.turnLeft)
  def turnRight: Agent = this.copy(orientation = orientation.turnRight)
  def forward(gridWidth: Int, gridHeight: Int): Agent = {
    val newAgentLocation: Coords = orientation match {
      case West => Coords(max(0, location.x - 1), location.y)
      case East => Coords(min(gridWidth - 1, location.x + 1), location.y)
      case South => Coords(location.x, max(0, location.y - 1))
      case North => Coords(location.x, min(gridHeight - 1, location.y + 1))
    }
    this.copy(location = newAgentLocation)
  }
}
