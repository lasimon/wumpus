package wumpusWorld

import wumpusWorld.agent.{Agent, BeelineAgent}
import wumpusWorld.environment.{Environment, Percept}
import eu.timepit.refined.auto._

object BeelineWorld {

  def main(args: Array[String]) = {

    def runEpisode(env: Environment, agent: Agent, percept: Percept): Double = {
      val (nextAgent, nextAction) = agent.nextAction(percept)
      println(s"Action: $nextAction")
      val (nextEnvironment: Environment, nextPercept: Percept) = env.applyAction(nextAction)
      println(nextEnvironment.visualize)
      println(nextPercept.show)
      nextPercept.reward + (if (!nextPercept.isTerminated) runEpisode(nextEnvironment, nextAgent, nextPercept) else 0.0)
    }

    val (initialEnv: Environment, initialPercept: Percept) = Environment(4, 4, 0.0, false)

    val agent = BeelineAgent(4, 4)
    val totalReward: Double = runEpisode(initialEnv, agent, initialPercept)
    println(s"Total reward: $totalReward")
  }
}
