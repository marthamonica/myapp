import play.Project._

name := "myapp"

version := "1.0"

playScalaSettings

libraryDependencies ++= Seq(
  jdbc,
  "mysql" % "mysql-connector-java" % "5.1.27",
  anorm
)