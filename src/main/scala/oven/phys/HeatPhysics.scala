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

  private[this] val contactSets = new HashSet[Set[HeatSource]]

  def heatSources: Seq[ HeatSource             ] = contactSets.flatten.toList
  def contacts   : Seq[(HeatSource, HeatSource)] = contactSets.toList.map(_.toList).map {case List(a, b) => a -> b}

  // Triggers
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
      contactStarted ~~(hss: Seq[(HeatSource, HeatSource)])~~> let hss.foreach {case (hs1, hs2) => contactSets += Set(hs1, hs2)}
      contactEnded   ~~(hss: Seq[(HeatSource, HeatSource)])~~> let hss.foreach {case (hs1, hs2) => contactSets -= Set(hs1, hs2)}

    // Do heat exchange between all the registered heat sources
    iteration = forOp: ";", (for ((hs1, hs2) <- contacts if hs1.temperature != hs2.temperature) yield [
      // Determine which one passes the heat to which one
      val equilibrium = hs1.equilibrium(hs2)  // Common temperature to converge to
      val donor      = if (equilibrium < hs1.temperature) hs1 else hs2  // The one to give away the heat
      val acceptor   = if (donor == hs1) hs2 else hs1  // The one to accept the heat

      val dt = donor.temperature - acceptor.temperature

      // How much heat to pass?
      val minConductivity = math.min(donor.heatConductivity, acceptor.heatConductivity) * step / 1000D
      val passingAbility  = math.min(donor.amountOfHeat, minConductivity)

      // Logistic function for smoothing speed up when the temperature converges (https://en.wikipedia.org/wiki/Logistic_function)
      val heatToPass = ( passingAbility / (1 + math.exp(-dt / (10 * passingAbility))) ) - passingAbility / 2

      // Do the transaction
      let donor   .amountOfHeat -= heatToPass
      let acceptor.amountOfHeat += heatToPass
    ])

    cleanup = let _time += step / 1000D
}