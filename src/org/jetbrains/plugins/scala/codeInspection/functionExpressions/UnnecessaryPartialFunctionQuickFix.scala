package org.jetbrains.plugins.scala.codeInspection.functionExpressions

import com.intellij.openapi.project.Project
import org.jetbrains.plugins.scala.codeInspection.functionExpressions.UnnecessaryPartialFunctionQuickFix._
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, InspectionBundle}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScCaseClause
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlockExpr
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

object UnnecessaryPartialFunctionQuickFix {
  val hint = InspectionBundle.message("convert.to.anonymous.function")
}

class UnnecessaryPartialFunctionQuickFix(caseClause: ScCaseClause, expression: ScBlockExpr)
  extends AbstractFixOnPsiElement(hint, caseClause) {

  override def doApplyFix(project: Project): Unit = {
    val expressionCopy = expression.copy().asInstanceOf[ScBlockExpr]
    expressionCopy.caseClauses.map(_.caseClauses).foreach {
      case Seq(clause) =>
        removeCaseKeyword(clause)
        expression.replace(
          ScalaPsiElementFactory.createExpressionFromText(
            expressionCopy.getText,
            expression.getManager))
    }
  }

  private def removeCaseKeyword(clause: ScCaseClause) =
    for {
      caseKeyword <- clause.firstChild
      whitespaceBeforePattern <- clause.pattern.map(_.getPrevSibling)
    } clause.deleteChildRange(caseKeyword, whitespaceBeforePattern)
}
