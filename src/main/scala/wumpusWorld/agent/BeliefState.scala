package wumpusWorld.agent

import wumpusWorld.environment.{Action, Percept}

trait BeliefState {
  def nextBeliefState(lastAction: Action, percept: Percept): BeliefState

}
