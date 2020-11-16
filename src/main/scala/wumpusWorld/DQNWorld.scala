package wumpusWorld

import me.shadaj.scalapy.py
import wumpusWorld.agent.{Agent, DQNAgent}
import wumpusWorld.environment.{Environment, Percept}
import eu.timepit.refined.auto._
import wumpusWorld.Bayes.Probability

object DQNWorld {

  def main(args: Array[String]): Unit = {
    // World size
    val gridWidth = 4
    val gridHeight = 4
    val pitProb: Probability = 0.2
    val epsilon = 0.3

    // Python libraries


    def runEpisode(env: Environment, agent: Agent, percept: Percept): Double = {
      val (nextAgent, nextAction) = agent.nextAction(percept)
      println(s"Action: $nextAction")
      val (nextEnvironment: Environment, nextPercept: Percept) = env.applyAction(nextAction)
      println(nextEnvironment.visualize)
      println(nextPercept.show)
      nextPercept.reward + (if (!nextPercept.isTerminated) runEpisode(nextEnvironment, nextAgent, nextPercept) else 0.0)
    }

    val (initialEnv: Environment, initialPercept: Percept) = Environment(gridWidth, gridHeight, 0.2, allowClimbWithoutGold = true)

    val qNetwork = DQNAgent.train(gridWidth, gridHeight, pitProb, 1000, epsilon, 50)

    val agent = DQNAgent(qNetwork, gridWidth, gridHeight, pitProb, epsilon)

    val totalReward: Double = runEpisode(initialEnv, agent, initialPercept)
    println(s"Total reward: $totalReward")
  }
}
