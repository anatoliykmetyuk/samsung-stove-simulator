package oven.phys

trait HeatSource {
  val name: String

  var _amountOfHeat = 0D
  def amountOfHeat  = if (!infiniteCapacity) _amountOfHeat else Double.MaxValue
  def amountOfHeat_=(x: Double) {if(!infiniteCapacity) _amountOfHeat = x}

  var _heatCapacity = 0D
  def heatCapacity  = if (!infiniteCapacity) _heatCapacity else Double.MaxValue
  def heatCapacity_=(x: Double) {if(!infiniteCapacity) _heatCapacity = x}


  var infiniteCapacity = false  // For unexhaustable heat sources, such as the environment or a heater
  var heatConductivity  = 0D     // Possibility to accept/donate the given amount of heat per second

  var _temperature = 0D
  def temperature  = if (!infiniteCapacity) amountOfHeat / heatCapacity else _temperature
  def temperature_=(t: Double) {
    if (!infiniteCapacity) amountOfHeat = t * heatCapacity
    else _temperature = t
  }

  /**
   * Common temperature to which the two will converge
   * should they have an infinite amount of time.
   */
  def equilibrium(that: HeatSource): Double =
    if (infiniteCapacity) temperature
    else if (that.infiniteCapacity) that.temperature
    else {
      val commonCapacity = heatCapacity + that.heatCapacity
      val k1 =      heatCapacity / commonCapacity
      val k2 = that.heatCapacity / commonCapacity

      temperature * k1 + that.temperature * k2
    }

  def onStepFinished(step: Long): Unit = {}

  override def toString(): String =
    s"[$name; amountOfHeat: $amountOfHeat; heatCapacity: $heatCapacity; heatConductivity: $heatConductivity; temperature: $temperature]"
}