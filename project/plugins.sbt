// Run sbt eclipse to create Eclipse project file
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")

// Run sbt xitrum-package to prepare for deploying to production environment
addSbtPlugin("tv.cntt" % "xitrum-package" % "1.9")

// For precompiling Scalate templates in the compile phase of SBT
addSbtPlugin("com.mojolly.scalate" % "xsbt-scalate-generator" % "0.5.0")
