scalaVersion := "2.11.7"
libraryDependencies += "org.subscript-lang" %% "subscript-swing" % "3.0.2-SNAPSHOT"
SubscriptSbt.projectSettings

initialCommands := """
import subscript.Predef._
import oven.phys._

val heater = new HeatSource {
  val name         = "Heater"
  heatCapacity     = 2
  heatConductivity = 20
  temperature      = 100
}

val plate = new HeatSource {
  val name         = "Plate"
  heatCapacity     = 1
  heatConductivity = 10
  temperature      = 0
}

val waterInPlate = new HeatSource {
  val name         = "Water"
  heatCapacity     = 1
  heatConductivity = 5
  temperature      = 0
}

val hp = new HeatPhysics(100)

hp.contactStarted.push((heater, plate))
hp.contactStarted.push((plate, waterInPlate))

runScript(hp.live)
"""