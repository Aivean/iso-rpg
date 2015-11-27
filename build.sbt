organization := "com.aivean"
name         := "iso-rpg"
version      := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"
//scalaVersion := "2.10.5"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

// Xitrum requires Java 7
javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

//------------------------------------------------------------------------------

libraryDependencies += "tv.cntt" %% "xitrum" % "3.25.0"

// Xitrum uses SLF4J, an implementation of SLF4J is needed
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"

// For writing condition in logback.xml
libraryDependencies += "org.codehaus.janino" % "janino" % "2.7.8"

libraryDependencies += "org.webjars" % "jquery" % "2.1.4"

libraryDependencies += "org.webjars" % "bootstrap" % "3.3.5"

// Scalate template engine config for Xitrum -----------------------------------

libraryDependencies += "tv.cntt" %% "xitrum-scalate" % "2.5"

// Precompile Scalate templates
seq(scalateSettings:_*)

ScalateKeys.scalateTemplateConfig in Compile := Seq(TemplateConfig(
  baseDirectory.value / "src" / "main" / "scalate",
  Seq(),
  Seq(Binding("helper", "xitrum.Action", true))
))

// xgettext i18n translation key string extractor is a compiler plugin ---------

autoCompilerPlugins := true
addCompilerPlugin("tv.cntt" %% "xgettext" % "1.3")
scalacOptions += "-P:xgettext:xitrum.I18n"

// Put config directory in classpath for easier development --------------------

// For "sbt console"
unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }

// For "sbt run"
unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }

// Copy these to target/xitrum when sbt xitrum-package is run
XitrumPackage.copy("config", "public", "script")
