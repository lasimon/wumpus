package wumpusWorld.agent

import wumpusWorld.environment.{Action, Forward, Percept}

class LogicalAgent extends Agent {


  val dumbDumb = new NaiveAgent();

  def nextAction(percept: Percept): Action = {
    Forward
  }

}
