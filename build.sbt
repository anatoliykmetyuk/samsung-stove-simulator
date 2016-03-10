scalaVersion := "2.11.7"
libraryDependencies += "org.subscript-lang" %% "subscript-swing" % "3.0.3"
SubscriptSbt.projectSettings

initialCommands := """
import subscript.Predef._
import oven.phys._

val heater = new HeatSource {
  val name         = "Heater"
  heatCapacity     = 5
  heatConductivity = 50
  temperature      = 100
  // infiniteCapacity = true
}

val plate = new HeatSource {
  val name         = "Plate"
  heatCapacity     = 2.5
  heatConductivity = 25
  temperature      = 0
}

val waterInPlate = new HeatSource {
  val name         = "Water"
  heatCapacity     = 1
  heatConductivity = 10
  temperature      = 0
}

val environment = new HeatSource {
  val name         = "Environment"
  infiniteCapacity = true
  heatConductivity = 5
  temperature      = 25
}

val hp = new HeatPhysics(100, Some(environment))

hp.contactStarted.push((heater, plate))
hp.contactStarted.push((plate, waterInPlate))

runScript(hp.live)
"""