Iso RPG
===

Attempt to create simple old-school isometric (MO)RPG using modern Scala technologies.

![Screenshot](screenshot.png)

Tech stack:

* [Scala](http://www.scala-lang.org/)
* [xitrum web framework](https://github.com/xitrum-framework/xitrum)
* [Akka](http://akka.io/)
* [Phaser JS](http://phaser.io/)
* [Phaser Isometric plugin](http://rotates.org/phaser/iso/)


To run
===

* Checkout this repo.
* Type: ``` sbt/sbt run ```  
* Open http://localhost:8080/


To generate Eclipse or IntelliJ project:
===

```
  sbt/sbt eclipse
  sbt/sbt gen-idea
```

[more info](https://xitrum-framework.github.io/guide/3.22/singlehtml/en/index.html#import-the-project-to-intellij)