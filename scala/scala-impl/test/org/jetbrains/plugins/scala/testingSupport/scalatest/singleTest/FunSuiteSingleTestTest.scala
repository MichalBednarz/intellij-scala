package org.jetbrains.plugins.scala.testingSupport.scalatest.singleTest

import org.jetbrains.plugins.scala.testingSupport.scalatest.generators.FunSuiteGenerator

trait FunSuiteSingleTestTest extends FunSuiteGenerator {

  val funSuiteTestPath = List("[root]", funSuiteClassName, "should run single test")
  val funSuiteTaggedTestPath = List("[root]", funSuiteClassName, "tagged")

  def testFunSuite(): Unit = {
    runTestByLocation2(9, 8, funSuiteFileName,
      assertConfigAndSettings(_, funSuiteClassName, "should run single test"),
      root => {
        assertResultTreeHasExactNamedPath(root, funSuiteTestPath)
        assertResultTreeDoesNotHaveNodes(root, "should not run other tests")
      }
    )
  }

  def testTaggedFunSuite(): Unit = {
    runTestByLocation2(12, 8, funSuiteFileName,
      assertConfigAndSettings(_, funSuiteClassName, "tagged"),
      root => {
        assertResultTreeHasExactNamedPath(root, funSuiteTaggedTestPath)
        assertResultTreeDoesNotHaveNodes(root, "should not run other tests")
      }
    )
  }
}
