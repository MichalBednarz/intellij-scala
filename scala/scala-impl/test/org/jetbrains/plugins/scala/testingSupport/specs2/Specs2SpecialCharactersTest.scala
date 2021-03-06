package org.jetbrains.plugins.scala.testingSupport.specs2

/**
 * @author Roman.Shein
 * @since 27.01.2015.
 */
abstract class Specs2SpecialCharactersTest extends Specs2TestCase {
  val testName = "Specs2SpecialCharactersTest"

  addSourceFile(s"$testName.scala",
    s"""import org.specs2.mutable.Specification
       |
       |class $testName extends Specification {
       | "Special characters test" should {
       |
       |   "Comma , test" in {
       |     success
       |   }
       |
       |   "! test" in {
       |     success
       |   }
       |
       |   "tick ' test" in {
       |     success
       |   }
       |
       |   "backtick ` test" in {
       |     success
       |   }
       |
       |   "tilde ~ test" in {
       |     success
       |   }
       | }
       |}""".stripMargin
  )

  def testComma(): Unit = {
    runTestByLocation2(5, 5, testName + ".scala",
      assertConfigAndSettings(_, testName, "Comma , test"),
      root => assertResultTreeHasExactNamedPath(root, Seq("[root]", testName, "Special characters test should", "Comma , test"))
    )
  }

  def testExclamation(): Unit = {
    runTestByLocation2(9, 5, testName + ".scala",
      assertConfigAndSettings(_, testName, "! test"),
      root => assertResultTreeHasExactNamedPath(root, Seq("[root]", testName, "Special characters test should", "! test")))
  }

  def testTick(): Unit = {
    runTestByLocation2(13, 5, testName + ".scala",
      assertConfigAndSettings(_, testName, "tick ' test"),
      root => assertResultTreeHasExactNamedPath(root, Seq("[root]", testName, "Special characters test should", "tick ' test")))
  }

  def testBacktick(): Unit = {
    runTestByLocation2(17, 5, testName + ".scala",
      assertConfigAndSettings(_, testName, "backtick ` test"),
      root => assertResultTreeHasExactNamedPath(root, Seq("[root]", testName, "Special characters test should", "backtick ` test")))
  }

  def testTilde(): Unit = {
    runTestByLocation2(21, 5, testName + ".scala",
      assertConfigAndSettings(_, testName, "tilde ~ test"),
      root => assertResultTreeHasExactNamedPath(root, Seq("[root]", testName, "Special characters test should", "tilde ~ test")))
  }
}
