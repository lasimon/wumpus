package wumpusWorld.agent

import wumpusWorld.environment.{Action}

trait Agent {
  def nextAction(beliefState: Option[BeliefState]): Action
}
