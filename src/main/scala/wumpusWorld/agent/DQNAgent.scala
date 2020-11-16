package wumpusWorld.agent

import me.shadaj.scalapy.interpreter.PyValue
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.SeqConverters
import me.shadaj.scalapy.py.Dynamic
import me.shadaj.scalapy.readwrite.Writer
import wumpusWorld.Bayes.Probability
import wumpusWorld.agent.DQNAgent.{np, randGen}
import wumpusWorld.environment._

import scala.annotation.tailrec
import scala.util.Random

case class DQNAgent(
                   qNetwork: Dynamic,
                     gridWidth: Int,
                     gridHeight: Int,
                     pitProb: Probability,
                   epsilon: Double,
                     agentState: AgentState = new AgentState,
                     breezeLocations: Set[Coords] = Set(), // locations a breeze has been sensed
                     stenchLocations: Set[Coords] = Set(), // locations a stench has been previously sensed
                     isGlitter: Boolean = false, // true if the agent perceives a Glitter
                     visitedLocations: Set[Coords] = Set(), // locations the agent has previously visited
                     heardScream: Boolean = false // true if a scream has been sensed at any previous timestep
                   ) extends Agent {

  // Random number generator
  private val randGen = new Random()

  // Python modules
  private val pyTorch = py.module("torch")
  private val np = py.module("numpy")

  def beliefStateAsModelTensor: Dynamic  = {
    def toInt(b: Boolean): Int = if (b) 1 else 0

    val agentLocationAsSeqOfInt: IndexedSeq[Int] = {
      for (row <- Range(0, gridHeight); col <- Range(0, gridWidth)) yield
        if (agentState.location.x == col && agentState.location.y == row) 1 else 0
    }
    val agentOrientationAsInt: IndexedSeq[Int]= {
      IndexedSeq(
        toInt(agentState.orientation == East),
        toInt(agentState.orientation == South),
        toInt(agentState.orientation == West),
        toInt(agentState.orientation == North)
      )
    }
    val visitedLocationsAsSeqOfInt: IndexedSeq[Int] = {
      for (row <- Range(0, gridHeight); col <- Range(0, gridWidth)) yield
        if (visitedLocations.contains(Coords(col, row))) 1 else 0
    }
    val stenchLocationsAsSeqOfInt: IndexedSeq[Int] = {
      for (row <- Range(0, gridHeight); col <- Range(0, gridWidth)) yield
        if (stenchLocations.contains(Coords(col, row))) 1 else 0
    }
    val breezeLocationsAsSeqOfInt: IndexedSeq[Int] = {
      for (row <- Range(0, gridHeight); col <- Range(0, gridWidth)) yield
        if (breezeLocations.contains(Coords(col, row))) 1 else 0
    }
    val allFeatures: IndexedSeq[Float] = (
      agentLocationAsSeqOfInt ++
        agentOrientationAsInt ++
        visitedLocationsAsSeqOfInt ++
        stenchLocationsAsSeqOfInt ++
        breezeLocationsAsSeqOfInt ++
        IndexedSeq(
          toInt(isGlitter),
          toInt(agentState.hasGold),
          toInt(agentState.hasArrow),
          toInt(heardScream)
        )
      ).map(f => f + randGen.nextGaussian().toFloat / 100.0f) // add a little noise

    val stateAsNpArray = np.array(allFeatures.toPythonProxy).reshape(1, 4 * gridWidth * gridHeight + 8)
    pyTorch.from_numpy(stateAsNpArray).float() // returns a pyTorch tensor of 32 bit floats of length inputSize
  }

  // Return an epsilon-greedy action given the current Q function and epsilon
  def nextEpsilonGreedyAction(percept: Percept): (DQNAgent, Action) = takeAction(percept, epsilon)

  def nextAction(percept: Percept): (DQNAgent, Action) = takeAction(percept, 0.0)

  // Return a greedy action given the current Q function and epsilon
  def takeAction(percept: Percept, epsilon: Double): (DQNAgent, Action) = {
    val newBreezeLocations = if (percept.breeze) breezeLocations + agentState.location else breezeLocations
    val newStenchLocations = if (percept.stench) stenchLocations + agentState.location else stenchLocations
    val newVisitedLocations = visitedLocations + agentState.location
    val stateTensor = beliefStateAsModelTensor // convert the belief state to a PyTorch tensor
    val qValues = qNetwork.forward(stateTensor).data.numpy() // use the nn to generate a Q prediction over the 6 actions
    val action = if (randGen.nextDouble < epsilon) Action.random else Action.fromInt(np.argmax(qValues).as[Int]) // take an epsilon-greedy step
    val newAgentState = agentState.applyAction(Some(action), gridWidth, gridHeight)
    val newAgent = this.copy(
      agentState = newAgentState,
      breezeLocations = newBreezeLocations,
      stenchLocations = newStenchLocations,
      visitedLocations = newVisitedLocations,
      isGlitter = percept.glitter,
      heardScream = heardScream || percept.scream
    )
    (newAgent, action)
  }
}

object DQNAgent {
  // Python modules
  private val np = py.module("numpy")
  private val pyCopy = py.module("copy")
  private val pyTorch = py.module("torch")
  private val pyCollections = py.module("collections")

  // Random number generator
  private val randGen = new Random()

  def train(gridWidth: Int, gridHeight: Int, pitProb: Probability, epochs: Int, epsilon: Double, iterationLimit: Int): Dynamic = {
    // DQN models
    val layers = Seq(4 * gridWidth * gridHeight + 8, 150, 100, 6)
    val qNetwork = pyTorch.nn.Sequential(
      pyTorch.nn.Linear(layers(0), layers(1)),
      pyTorch.nn.ReLU(),
      pyTorch.nn.Linear(layers(1), layers(2)),
      pyTorch.nn.ReLU(),
      pyTorch.nn.Linear(layers(2), layers(3))
    )

    val targetNetwork = pyCopy.deepcopy(qNetwork).load_state_dict(qNetwork.state_dict())

    val lossFunction = pyTorch.nn.MSELoss()
    val learningRate = 1e-3
    val optimizer = pyTorch.optim.Adam(qNetwork.parameters(), lr=learningRate)
    val batchSize = 200

    println(s"CUDA is available: ${pyTorch.cuda.is_available()}")

    case class ReplayBufferEntry(
      previousAgentState: DQNAgent,
      action: Action,
      reward: Float,
      subsequentAgentState: DQNAgent,
      isTerminated: Boolean
  )
//    implicit object ScalaPyDynamicConverter extends Writer[Dynamic] {
//      implicit def write(d: PyValue) = d
//    }

    for (i <- Range(0, epochs)) {
      val (initialEnv: Environment, initialPercept: Percept) = Environment(gridWidth, gridHeight, pitProb, allowClimbWithoutGold = true) // create an initialized environment

      @tailrec
      def act(env: Environment, percept: Percept, iteration: Int, agent: DQNAgent, replay: List[ReplayBufferEntry]): Unit = {
        val (newAgent: DQNAgent, action: Action) = agent.nextEpsilonGreedyAction(percept)
        val (newEnv, newPercept) = env.applyAction(action)
        val reward = newPercept.reward
        val newReplay = ReplayBufferEntry(agent, action, reward.toFloat, newAgent, newPercept.isTerminated) :: replay // add new observation to head of list (an efficient operation)

        println(s"replay length: ${replay.length}")
        if (newReplay.length > batchSize) {
          val randomIndices = for (_ <- 1 to batchSize; r = randGen.nextInt(newReplay.length)) yield r
          val miniBatch: List[ReplayBufferEntry] = replay.zipWithIndex.filter { case (_, index) => randomIndices.contains(index) }.map { case (entry, _) => entry }
          val previousStates: List[Dynamic] = miniBatch.map(_.previousAgentState.beliefStateAsModelTensor)
          val previousStateBatch = pyTorch.cat(previousStates.toPythonProxy)
          val qForPreviousState = qNetwork.forward(previousStateBatch)

          val subsequentStates: List[Dynamic] = miniBatch.map(_.subsequentAgentState.beliefStateAsModelTensor)
          val subsequentStateBatch = pyTorch.cat(subsequentStates.toPythonProxy)
          val qForSubsequentStateFromTargetNetwork = targetNetwork.no_grad().forward(subsequentStateBatch)

          val y: List[Float] = miniBatch.map(entry => entry.reward + (if (entry.isTerminated) 0.0f else pyTorch.max(qForSubsequentStateFromTargetNetwork, dim = 1).squeeze().as[Float]))
          val actions = miniBatch.map(_.action.toInt)
          val actionBatch = pyTorch.tensor(actions.toPythonProxy)
          val x = qForPreviousState.gather(dim = 1, index = actionBatch.long().unsqueeze(dim = 1)).squeeze()

        }
        println(s"epoch: ${i + 1}, iteration: $iteration, action: $action")
        if (newPercept.isTerminated || iteration > iterationLimit)
          Unit
        else
          act(newEnv, newPercept, iteration + 1, newAgent, newReplay)
      }

      act(initialEnv, initialPercept, 0, DQNAgent(qNetwork, gridWidth, gridHeight, pitProb, epsilon), List.empty)
    }
    qNetwork
  }
}
