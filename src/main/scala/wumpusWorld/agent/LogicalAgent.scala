package wumpusWorld.agent

import wumpusWorld.environment.{Action, Forward, Percept}
import me.shadaj.scalapy.py

class LogicalAgent extends Agent {


  val dumbDumb = new NaiveAgent();

  def nextAction(percept: Percept): Action = {
    println(py.global.len(List(1, 2, 3)))
    Forward
  }

}
