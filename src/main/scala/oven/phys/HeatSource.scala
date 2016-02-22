package oven.phys

trait HeatSource {
  val name: String

  var _amountOfHeat = 0D
  def amountOfHeat  = if (!infiniteCapacity) _amountOfHeat else Double.MaxValue
  def amountOfHeat_=(x: Double) {_amountOfHeat = x}

  var _heatCapacity = 0D
  def heatCapacity  = if (!infiniteCapacity) _heatCapacity else Double.MaxValue
  def heatCapacity_=(x: Double) {_heatCapacity = x}


  var infiniteCapacity = false  // For unexhaustable heat sources, such as the environment or a heater
  var heatConductivity  = 0D     // Possibility to accept/donate the given amount of heat per second

  var _temperature = 0D
  def temperature  = if (!infiniteCapacity) amountOfHeat / heatCapacity else _temperature
  def temperature_=(t: Double) {
    _temperature = t
    amountOfHeat = t * heatCapacity
  }

  override def toString(): String =
    s"[$name; amountOfHeat: $amountOfHeat; heatCapacity: $heatCapacity; heatConductivity: $heatConductivity; temperature: $temperature]"
}
