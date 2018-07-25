
val sharedSettings = Seq(
  scalaVersion := "2.12.6"
)
val agent = project
  .settings(sharedSettings: _*)
  .settings(
    packageOptions in (Compile, packageBin) +=
      Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(sharedSettings: _*)
  .settings(
    fork in run := true,

    libraryDependencies += "com.lihaoyi" % "ammonite_2.12.6" % "1.1.2",
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
)
