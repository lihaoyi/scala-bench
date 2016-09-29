
val sharedSettings = Seq(
  scalaVersion := "2.11.8"
)
val agent = project
  .settings(
    sharedSettings,
    packageOptions in (Compile, packageBin) += 
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(
    sharedSettings,
    fork in run := true,

    libraryDependencies += "com.lihaoyi" % "ammonite_2.11.8" % "0.7.7",
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
)
