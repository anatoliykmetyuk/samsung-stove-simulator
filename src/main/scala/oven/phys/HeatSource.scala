package oven.phys

trait HeatSource {
  val name: String

  var amountOfHeat      = 0D
  var heatCapacity      = 0D
  var inifiniteCapacity = false  // For unexhaustable heat sources, such as the environment or a heater
  var heatConductivity  = 0D     // Possibility to accept/donate the given amount of heat per second

  def temperature = amountOfHeat / heatCapacity

  def temperature_=(t: Double) {amountOfHeat = t * heatCapacity}

  /**
   * Common temperature to which the two will converge
   * should they have an infinite amount of time.
   */
  def equilibrium(that: HeatSource): Double = {
    val commonCapacity = heatCapacity + that.heatCapacity
    val k1 =      heatCapacity / commonCapacity
    val k2 = that.heatCapacity / commonCapacity

    temperature * k1 + that.temperature * k2
  }

  def onStepFinished(step: Long): Unit = {}

  override def toString(): String =
    s"[$name; amountOfHeat: $amountOfHeat; heatCapacity: $heatCapacity; heatConductivity: $heatConductivity; temperature: $temperature]"
}