package org.jetbrains.plugins.scala
package testingSupport.test

import com.intellij.lang.Language
import com.intellij.openapi.diagnostic.Logger
import com.intellij.psi.{PsiClass, PsiElement}
import javax.swing.Icon
import org.jetbrains.plugins.scala.extensions.LoggerExt
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScTemplateDefinition, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.light.PsiClassWrapper
import org.jetbrains.plugins.scala.lang.psi.{ElementScope, ScalaPsiUtil}

abstract class AbstractTestFramework extends JavaTestFrameworkBridge {

  private val Log: Logger = Logger.getInstance(this.getClass)

  def baseSuitePaths: Seq[String]

  override def getIcon: Icon = Icons.SCALA_TEST

  override def getLanguage: Language = ScalaLanguage.INSTANCE

  override def isTestMethod(element: PsiElement): Boolean = false

  override final def isTestClass(clazz: PsiClass, canBePotential: Boolean): Boolean = {
    val definition: ScTemplateDefinition = clazz match {
      case PsiClassWrapper(definition)  => definition
      case definition: ScTypeDefinition => definition
      case _                            => return false
    }
    isTestClass(definition)
  }

  protected def isTestClass(definition: ScTemplateDefinition): Boolean = {
    val elementScope = ElementScope(definition.getProject)

    def isInheritor(baseSuitePath: String): Boolean = {
      val cachedClass = elementScope.getCachedClass(baseSuitePath)
      cachedClass.exists(ScalaPsiUtil.isInheritorDeep(definition, _))
    }

    val cachedMarkerClass = elementScope.getCachedClass(getMarkerClassFQName)
    if (cachedMarkerClass.isDefined) {
      baseSuitePaths.exists(isInheritor)
    } else {
      Log.traceSafe(s"can't find marker class $getMarkerClassFQName for class ${definition.name}")
      false
    }
  }

  /** @return template file name from scala/scala-impl/resources/fileTemplates/code, used in "create test dialog" */
  def testFileTemplateName: String = "ScalaTest Class"
}

object AbstractTestFramework {

  final case class TestFrameworkSetupInfo(dependencies: Seq[String], scalacOptions: Seq[String])
}