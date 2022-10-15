ThisBuild / scalaVersion := "3.2.0"

val catsVersion = "2.8.0"
val catsEffectVersion = "3.3.14"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion
)