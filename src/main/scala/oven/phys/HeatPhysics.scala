package oven
package phys

import subscript.language
import subscript.Predef._
import subscript.objectalgebra._

import collection.mutable.ListBuffer

import Util._

/**
 * Step is a simulation step in milliseconds.
 */
class HeatPhysics(val step: Long = 100) extends SSProcess {

  // Internal state
  private[this] var _time: Double = 0
  def time = _time

  private[this] val heatSources = new ListBuffer[    HeatSource ]
  private[this] val contactSets = new ListBuffer[Set[HeatSource]]

  // Triggers
  val heatSourceConnected    = new Signal[ HeatSource             ]
  val heatSourceDisconnected = new Signal[ HeatSource             ]
  val contactStarted         = new Signal[(HeatSource, HeatSource)]
  val contactEnded           = new Signal[(HeatSource, HeatSource)]

  /** All the heat sources in contact with this one. */
  def contacts(hs: HeatSource): Seq[HeatSource] =
    contactSets.filter {_.contains(hs)}.flatMap(_.toSeq).filter(_ != hs)

  script..
    live = controls iteration cleanup debug sleep: step ...

    debug =
      println("=== Time: " + time + "s ===")

      println("=== Heat Sources Dump ===")
      let heatSources.foreach(x => println(x))

      println("=== Contacts Dump ===")
      let contactSets.foreach(x => println(x))

      println("=== Dump End ===\n\n")

    controls =;&&
      heatSourceConnected    ~~(hss: Seq[ HeatSource             ])~~> let hss.foreach(hs => heatSources += hs)
      heatSourceDisconnected ~~(hss: Seq[ HeatSource             ])~~> let hss.foreach(hs => heatSources -= hs)
      contactStarted         ~~(hss: Seq[(HeatSource, HeatSource)])~~> let hss.foreach {case (hs1, hs2) => contactSets += Set(hs1, hs2)}
      contactEnded           ~~(hss: Seq[(HeatSource, HeatSource)])~~> let hss.foreach {case (hs1, hs2) => contactSets -= Set(hs1, hs2)}

    // Do heat exchange between all the registered heat sources
    iteration =
      var alreadyProcessed: Seq[Set[HeatSource]] = Nil
      forOp: ";", (for (hs1 <- heatSources; hs2 <- contacts(hs1) if !alreadyProcessed.contains(Set(hs1, hs2)) && hs1.temperature != hs2.temperature) yield [
        val equilibrium = hs1.equilibrium(hs2)  // Common temperature to converge to
        val donor      = if (equilibrium < hs1.temperature) hs1 else hs2  // The one to give away the heat
        val acceptor   = if (donor == hs1) hs2 else hs1  // The one to accept the heat

        // How much heat to pass?
        val minConductivity = math.min(donor.heatConductivity, acceptor.heatConductivity) * step / 1000D
        val heatToPass      = math.min(donor.amountOfHeat, minConductivity)

        // Do the transaction
        let donor   .amountOfHeat -= heatToPass
        let acceptor.amountOfHeat += heatToPass

        let alreadyProcessed :+= Set(donor, acceptor)
      ])

    cleanup = let _time += step / 1000D
}