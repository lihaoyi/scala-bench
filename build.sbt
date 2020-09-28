
val sharedSettings = Seq(
  scalaVersion := "2.13.3"
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

    libraryDependencies +=  "com.lihaoyi" % "ammonite_2.13.3" % "2.2.0",
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
)
