package oven
package phys

import subscript.language
import subscript.Predef._
import subscript.objectalgebra._

import collection.mutable.HashSet

import Util._

/**
 * Step is a simulation step in milliseconds.
 */
class HeatPhysics(val step: Long = 100, val environment: Boolean = false, val environmentalTemperature: Double = 30) extends SSProcess {

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


    // Do heat exchange between all the registered heat sources
    iteration = forOp: ";", (for ((hs1, hs2) <- contacts if hs1.temperature != hs2.temperature) yield [
      // Determine which one passes the heat to which one
      val dt       = hs1.temperature - hs2.temperature
      val donor    = if (dt    >= 0  ) hs1 else hs2  // The one to give away the heat
      val acceptor = if (donor == hs1) hs2 else hs1  // The one to accept the heat

      // How much heat to pass?
      val minConductivity = math.min(hs1.heatConductivity, hs2.heatConductivity) * step / 1000D
      val passingAbility  = math.min(donor.amountOfHeat, minConductivity)

      // Logistic function for smoothing speed up when the temperature converges (https://en.wikipedia.org/wiki/Logistic_function)
      val heatToPass = ( passingAbility / (1 + math.exp(-dt / (10 * passingAbility))) ) - passingAbility / 2

      // Do the transaction
      let donor   .amountOfHeat -= heatToPass
      let acceptor.amountOfHeat += heatToPass
    ])

    cleanup = let _time += step / 1000D
}