organization := "com.apple"

name := "cassandra_quickcheck"

version := "0.0.0-SNAPSHOT"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.apache.cassandra" %  "cassandra-all" % "42.0.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
