package wumpusWorld

import wumpusWorld.agent.{Agent, ProbAgent}
import wumpusWorld.environment.{AgentState, Coords, East, Environment, Percept}
import eu.timepit.refined.auto._

object ProbWorld {

  def main(args: Array[String]) = {

    def runEpisode(env: Environment, agent: Agent, percept: Percept): Double = {
      val (nextAgent, nextAction) = agent.nextAction(percept)
      println(s"Action: $nextAction")
      val (nextEnvironment: Environment, nextPercept: Percept) = env.applyAction(nextAction)
      println(nextEnvironment.visualize)
      println(nextPercept.show)
      nextPercept.reward + (if (!nextPercept.isTerminated) runEpisode(nextEnvironment, nextAgent, nextPercept) else 0.0)
    }

    val (initialEnv: Environment, initialPercept: Percept) = Environment(4, 4, 0.2, true)

    val agent = ProbAgent(4, 4,  0.2)
    val totalReward: Double = runEpisode(initialEnv, agent, initialPercept)
    println(s"Total reward: $totalReward")
  }
}
