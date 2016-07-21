package org.jetbrains.plugins.scala.codeInspection.unusedInspections

import com.intellij.codeInsight.FileModificationService
import com.intellij.codeInspection.LocalQuickFixAndIntentionActionOnPsiElement
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPatternList
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScNamingPattern, ScReferencePattern, ScTypedPattern}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

class DeleteUnusedElementFix(e: ScNamedElement) extends LocalQuickFixAndIntentionActionOnPsiElement(e) {
  override def getText: String = DeleteUnusedElementFix.Hint

  override def getFamilyName: String = getText

  override def invoke(project: Project, file: PsiFile, editor: Editor, startElement: PsiElement, endElement: PsiElement): Unit = {
    if (FileModificationService.getInstance.prepareFileForWrite(startElement.getContainingFile)) {
      startElement match {
        case ref: ScReferencePattern => ref.getContext match {
          case pList: ScPatternList if pList.patterns == Seq(ref) =>
            val context: PsiElement = pList.getContext
            context.getContext.deleteChildRange(context, context)
          case pList: ScPatternList if pList.allPatternsSimple && pList.patterns.startsWith(Seq(ref)) =>
            val end = ref.nextSiblings.find(_.getNode.getElementType == ScalaTokenTypes.tCOMMA).get.getNextSiblingNotWhitespace.getPrevSibling
            pList.deleteChildRange(ref, end)
          case pList: ScPatternList if pList.allPatternsSimple =>
            val start = ref.prevSiblings.find(_.getNode.getElementType == ScalaTokenTypes.tCOMMA).get.getPrevSiblingNotWhitespace.getNextSibling
            pList.deleteChildRange(start, ref)
          case _ =>
            // val (a, b) = t
            // val (_, b) = t
            val anonymousRefPattern = ScalaPsiElementFactory.createWildcardPattern(ref.getManager)
            ref.replace(anonymousRefPattern)
        }
        case typed: ScTypedPattern =>
          val wildcard = ScalaPsiElementFactory.createWildcardNode(typed.getManager).getPsi
          typed.nameId.replace(wildcard)
        case naming: ScNamingPattern =>
          naming.replace(naming.named)
        case _ => startElement.delete()
      }
    }
  }
}

object DeleteUnusedElementFix {
  val Hint: String = "Remove unused element"
}
