package wumpusWorld

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.W

object Bayes {
    type Probability = Double Refined Interval.Closed[W.`0.0`.T, W.`1.0`.T]
}
