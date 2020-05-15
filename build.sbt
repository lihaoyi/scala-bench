val scalaVersions = Seq("2.11.12","2.12.11", "2.13.2")
val agent = project
  .settings(
    crossScalaVersions := scalaVersions,
    packageOptions in (Compile, packageBin) += 
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(
    crossScalaVersions := scalaVersions,
    fork in run := true,
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0",
    libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.8",
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value),
    mainClass in assembly := Some("bench.PerfMain")
)
