package org.jetbrains.plugins.scala

import junit.framework.TestCase
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel
import org.junit.Assert._

class ScalaVersionTest extends TestCase {

  def testParseFromString(): Unit = {
    assertEquals(Some(new ScalaVersion(ScalaLanguageLevel.Scala_2_9, "0")), ScalaVersion.fromString("2.9.0"))
    assertEquals(Some(new ScalaVersion(ScalaLanguageLevel.Scala_2_9, "3")), ScalaVersion.fromString("2.9.3"))
    assertEquals(Some(new ScalaVersion(ScalaLanguageLevel.Scala_2_12, "10")), ScalaVersion.fromString("2.12.10"))
    assertEquals(Some(new ScalaVersion(ScalaLanguageLevel.Scala_2_13, "2")), ScalaVersion.fromString("2.13.2"))
    assertEquals(Some(new ScalaVersion(ScalaLanguageLevel.Scala_2_13, "3-RC1")), ScalaVersion.fromString("2.13.3-RC1"))

    assertEquals(None, ScalaVersion.fromString("A.BC.3"))
    assertEquals(None, ScalaVersion.fromString("2.BC.3"))
    assertEquals(None, ScalaVersion.fromString("A.13.3"))
  }
}