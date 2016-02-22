package oven
package phys

import subscript.language
import subscript.Predef._
import subscript.objectalgebra._

import collection.mutable.HashSet

import Util._

/**
 * Step is a simulation step in milliseconds.
 * The larger the convergence speed, the SLOWER the temperatures converge.
 */
class HeatPhysics(val step: Long = 100, val environment: Option[HeatSource] = None, val convergenceSpeed: Int = 1) extends SSProcess {

  // Internal state
  private[this] var _time: Double = 0
  def time = _time

  private[this] val heatSources = new HashSet[    HeatSource ]
  private[this] var contactSets = new HashSet[Set[HeatSource]]

  def contacts: Seq[(HeatSource, HeatSource)] = contactSets.toList.map(_.toList).map {case List(a, b) => a -> b}

  // Triggers
  val sourceAdded    = new Signal[HeatSource]
  val sourceRemoved  = new Signal[HeatSource]
  val contactStarted = new Signal[(HeatSource, HeatSource)]
  val contactEnded   = new Signal[(HeatSource, HeatSource)]

  script..
    live = doStep sleep: step ...

    doStep = controls iteration cleanup debug

    debug =
      println("=== Time: " + time + "s ===")

      println("=== Heat Sources Dump ===")
      let heatSources.foreach(x => println(x))

      println("=== Contacts Dump ===")
      let contactSets.foreach(x => println(x))

      println("=== Dump End ===\n\n")

    controls =;&&
      contactStarted ~~(hss: Seq[(HeatSource, HeatSource)])~~> let hss.foreach {case (hs1, hs2) =>
        val pair = Set(hs1, hs2)
        contactSets  += pair
        heatSources ++= pair
      }

      contactEnded   ~~(hss: Seq[(HeatSource, HeatSource)])~~> let hss.foreach {case (hs1, hs2) => contactSets -= Set(hs1, hs2)}
      sourceAdded    ~~(hss: Seq[ HeatSource             ])~~> let hss.foreach {hs => heatSources += hs}
      
      sourceRemoved  ~~(hss: Seq[HeatSource])~~> let hss.foreach {hs =>
        heatSources -= hs
        contactSets  = contactSets.filter(!_.contains(hs))
      }


    // Do heat exchange between all the registered heat sources and the environment
    iteration =
      forOp: ";", (for ((hs1, hs2) <- contacts if hs1.temperature != hs2.temperature) yield [heatTransaction: hs1, hs2])
      if environment.isDefined then forOp: ";", (for (hs <- heatSources.toList if hs.temperature != environment.get.temperature) yield [heatTransaction: environment.get, hs ])

    heatTransaction(hs1: HeatSource, hs2: HeatSource) =
      val dt            = hs1.temperature - hs2.temperature

      // How much heat to pass?
      val availableHeat   = if (dt >= 0) hs1.amountOfHeat else hs2.amountOfHeat
      val minConductivity = math.min(hs1.heatConductivity, hs2.heatConductivity) * step / 1000D
      val passingAbility  = math.min(availableHeat, minConductivity)

      // Logistic function for smoothing speed up when the temperature converges (https://en.wikipedia.org/wiki/Logistic_function)
      val heatToPass = ( passingAbility / (1 + math.exp(-dt / (convergenceSpeed * passingAbility))) ) - passingAbility / 2

      // Do the transaction
      let hs1.amountOfHeat -= heatToPass
      let hs2.amountOfHeat += heatToPass

    cleanup = let _time += step / 1000D
}