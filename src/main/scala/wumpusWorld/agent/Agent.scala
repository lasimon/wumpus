package wumpusWorld.agent

import wumpusWorld.environment.{Action, Percept}

trait Agent {
  def nextAction(percept: Percept): Action
}
