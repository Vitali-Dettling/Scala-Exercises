import StudentBuild.assignment

name := course.value + "-" + assignment.value

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies ++= assignmentsMap.value.values.flatMap(_.dependencies).toSeq
libraryDependencies += "ch.epfl.lamp" % "scala-grading-runtime_2.11" % "0.3"

// include the common dir
commonSourcePackages += "common"

courseId := "PeZYFz-zEeWB_AoW1KYI4Q"

val depsQuickcheck = Seq("org.scalacheck" %% "scalacheck" % "1.12.1")

assignmentsMap := {
  val styleSheetPath = (baseDirectory.value / ".." / ".." / "project" / "scalastyle_config.xml").getPath
  Map(
    "example" -> Assignment(
      packageName = "example",
      key = "lLkU5d7xEeWGkg7lknKHZw",
      itemId = "AYDPu",
      partId = "5QFuy",
      maxScore = 10d,
      styleScoreRatio = 0.2,
      styleSheet = styleSheetPath,
      dependencies = depsQuickcheck)
  )
}
