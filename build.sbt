scalaVersion := "2.11.7"
libraryDependencies += "org.subscript-lang" %% "subscript-swing" % "3.0.2-SNAPSHOT"
SubscriptSbt.projectSettings

initialCommands := """
import oven.phys._

val brick = new HeatSource {
  val name = "Brick"
  amountOfHeat = 0
  heatCapacity = 2
}

val water = new HeatSource {
  val name = "Water"
  amountOfHeat = 100
  heatCapacity = 1
}
"""