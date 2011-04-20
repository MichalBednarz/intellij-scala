package org.jetbrains.plugins.scala
package lang
package psi

import api.base._
import api.base.types.{ScRefinement, ScExistentialClause, ScTypeElement}
import api.toplevel.packaging.{ScPackaging, ScPackageContainer}
import api.toplevel.{ScEarlyDefinitions, ScTypedDefinition}
import api.toplevel.imports.usages.ImportUsed
import api.toplevel.imports.{ScImportStmt, ScImportExpr, ScImportSelector, ScImportSelectors}
import api.{ScPackageLike, ScPackage, ScalaFile, ScalaRecursiveElementVisitor}
import com.intellij.psi.scope.PsiScopeProcessor
import api.toplevel.templates.ScTemplateBody
import api.toplevel.typedef._
import impl.expr.ScBlockExprImpl
import impl.toplevel.typedef.{MixinNodes, TypeDefinitionMembers}
import implicits.ScImplicitlyConvertible
import com.intellij.openapi.progress.ProgressManager
import api.expr._
import api.expr.xml.ScXmlExpr

import com.intellij.openapi.project.Project
import _root_.org.jetbrains.plugins.scala.lang.psi.types._
import api.statements._
import com.intellij.psi._
import codeStyle.CodeStyleSettingsManager
import com.intellij.psi.search.GlobalSearchScope
import lang.psi.impl.ScalaPsiElementFactory
import nonvalue.{Parameter, TypeParameter, ScTypePolymorphicType}
import patterns.{ScBindingPattern, ScReferencePattern, ScCaseClause}
import stubs.ScModifiersStub
import types.Compatibility.Expression
import params._
import parser.parsing.expressions.InfixExpr
import parser.util.ParserUtils
import result.{TypingContext, Success, TypeResult}
import structureView.ScalaElementPresentation
import com.intellij.util.ArrayFactory
import com.intellij.psi.util._
import formatting.settings.ScalaCodeStyleSettings
import com.intellij.openapi.roots.{ProjectRootManager, ProjectFileIndex}
import com.intellij.openapi.module.Module
import lang.resolve.processor._
import lang.resolve.{ResolveTargets, ResolveUtils, ScalaResolveResult}
import com.intellij.psi.impl.light.LightModifierList
import collection.mutable.{HashSet, ArrayBuffer}
import collection.immutable.{HashMap, Stream}
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.stubs.StubElement
import org.jetbrains.plugins.scala.extensions._

/**
 * User: Alexander Podkhalyuzin
 */
object ScalaPsiUtil {
  def fileContext(psi: PsiElement): PsiFile = {
    if (psi == null) return null
    psi match {
      case f: PsiFile => f
      case _ => fileContext(psi.getContext)
    }
  }

  def tuplizy(s: Seq[Expression], project: Project, scope: GlobalSearchScope): Option[Seq[Expression]] = {
    val exprTypes: Seq[ScType] =
      s.map(_.getTypeAfterImplicitConversion(true, false, None)).map {
        case (res, _) => res.getOrElse(Any)
      }
    val qual = "scala.Tuple" + exprTypes.length
    val tupleClass = JavaPsiFacade.getInstance(project).findClass(qual, scope)
    if (tupleClass == null)
      return None
    else
      Some(Seq(new Expression(ScParameterizedType(ScDesignatorType(tupleClass), exprTypes))))
  }

  def getNextSiblingOfType[T <: PsiElement](sibling: PsiElement, aClass: Class[T]): T = {
    if (sibling == null) return null.asInstanceOf[T]
    var child: PsiElement = sibling.getNextSibling
    while (child != null) {
      if (aClass.isInstance(child)) {
        return child.asInstanceOf[T]
      }
      child = child.getNextSibling
    }
    return null.asInstanceOf[T]
  }

  def processImportLastParent(processor: PsiScopeProcessor, state: ResolveState, place: PsiElement,
                              lastParent: PsiElement, typeResult: => TypeResult[ScType]): Boolean = {
    val subst = state.get(ScSubstitutor.key).toOption.getOrElse(ScSubstitutor.empty)
    lastParent match {
      case _: ScImportStmt => {
        typeResult match {
          case Success(t, _) => {
            (processor, place) match {
              case (b: BaseProcessor, p: ScalaPsiElement) => b.processType(subst subst t, p, state)
              case _ => true
            }
          }
          case _ => true
        }
      }
      case _ => true
    }
  }

  def findImplicitConversion(e: ScExpression, refName: String, kinds: collection.Set[ResolveTargets.Value],
                             ref: PsiElement, processor: BaseProcessor):
    Option[(ScType, PsiNamedElement, collection.Set[ImportUsed])] = {
    lazy val exprType = e.getTypeWithoutImplicits(TypingContext.empty)
    //TODO! remove this after find a way to improve implicits according to compiler.
    val isHardCoded = refName == "+" && exprType.map(_.isInstanceOf[ValType]).getOrElse(false)
    val mp = e.implicitMap()
    var implicitMap: Seq[(ScType, PsiNamedElement, scala.collection.Set[ImportUsed])] = mp.filter({
      case (t: ScType, fun: PsiNamedElement, importsUsed: collection.Set[ImportUsed]) => {
        ProgressManager.checkCanceled
        if (!isHardCoded || !t.isInstanceOf[ValType]) {
          val newProc = new ResolveProcessor(kinds, ref, refName)
          newProc.processType(t, e, ResolveState.initial)
          !newProc.candidatesS.isEmpty
        } else false
      }
    })
    if (implicitMap.length != 1 && processor.isInstanceOf[MethodResolveProcessor]) {
      implicitMap = implicitMap.filter({
        case (t: ScType, fun: PsiNamedElement, importsUsed: collection.Set[ImportUsed]) => {
          val mrp = processor.asInstanceOf[MethodResolveProcessor]
          val newProc = new MethodResolveProcessor(ref, refName, mrp.argumentClauses, mrp.typeArgElements, kinds,
            mrp.expectedOption, mrp.isUnderscore, mrp.isShapeResolve, mrp.constructorResolve)
          val tp = t
          newProc.processType(tp, e, ResolveState.initial)
          val cand = newProc.candidatesS
          val filtered = !cand.filter(_.isApplicable).isEmpty
          filtered
        }
      })
    }
    val mostSpecificImplicit = if (implicitMap.length == 0) return None
    else if (implicitMap.length == 0) implicitMap.apply(0)
    else MostSpecificUtil(ref, 1).mostSpecificForImplicit(implicitMap.toSet).getOrElse(return None)
    Some(mostSpecificImplicit)
  }

  def findCall(place: PsiElement): Option[ScMethodCall] = {
    place.getContext match {
      case call: ScMethodCall => Some(call)
      case p: ScParenthesisedExpr => findCall(p)
      case g: ScGenericCall => findCall(g)
      case _ => None
    }
  }

  def processTypeForUpdateOrApplyCandidates(call: ScMethodCall, tp: ScType, isShape: Boolean,
                                            noImplicits: Boolean): Array[ScalaResolveResult] = {
    import call._
    val isUpdate = call.isUpdateCall
    val methodName = if (isUpdate) "update" else "apply"
    val args: Seq[ScExpression] = call.args.exprs ++ (
            if (isUpdate) getContext.asInstanceOf[ScAssignStmt].getRExpression match {
              case Some(x) => Seq[ScExpression](x)
              case None =>
                Seq[ScExpression](ScalaPsiElementFactory.createExpressionFromText("{val x: Nothing = null; x}",
                  getManager)) //we can't to not add something => add Nothing expression
            }
            else Seq.empty)
    val typeArgs: Seq[ScTypeElement] = getEffectiveInvokedExpr match {
      case gen: ScGenericCall => gen.arguments
      case _ => Seq.empty
    }
    import Compatibility.Expression._
    val processor = new MethodResolveProcessor(getEffectiveInvokedExpr, methodName, args :: Nil, typeArgs,
      isShapeResolve = isShape, enableTupling = true)
    processor.processType(tp.inferValueType, getEffectiveInvokedExpr, ResolveState.initial)
    var candidates = processor.candidatesS

    if (!noImplicits && candidates.forall(!_.isApplicable)) {
      //should think about implicit conversions
      for (t <- getEffectiveInvokedExpr.getImplicitTypes) {
        ProgressManager.checkCanceled
        val importsUsed = getEffectiveInvokedExpr.getImportsForImplicit(t)
        var state = ResolveState.initial.put(ImportUsed.key, importsUsed)
        getEffectiveInvokedExpr.getClazzForType(t) match {
          case Some(cl: PsiClass) => state = state.put(ScImplicitlyConvertible.IMPLICIT_RESOLUTION_KEY, cl)
          case _ =>
        }
        processor.processType(t, getEffectiveInvokedExpr, state)
      }
      candidates = processor.candidatesS
    }
    candidates.toArray
  }

  def processTypeForUpdateOrApply(tp: ScType, call: ScMethodCall, isShape: Boolean): Option[ScType] = {
    import call._

    def checkCandidates(withImplicits: Boolean): Option[ScType] = {
      val candidates = processTypeForUpdateOrApplyCandidates(call, tp, isShape, noImplicits = !withImplicits)
      PartialFunction.condOpt(candidates) {
        case Array(ScalaResolveResult(fun: PsiMethod, s: ScSubstitutor)) => fun match {
          case fun: ScFun => s.subst(fun.polymorphicType)
          case fun: ScFunction => s.subst(fun.polymorphicType)
          case meth: PsiMethod => ResolveUtils.javaPolymorphicType(meth, s, getResolveScope)
        }
      }
    }

    checkCandidates(withImplicits = false).orElse(checkCandidates(withImplicits = true))
  }

  def isAnonymousExpression(expr: ScExpression): (Int, ScExpression) = {
    expr.getText.indexOf("_") match {case -1 => case _ => {
        val seq = ScUnderScoreSectionUtil.underscores(expr)
        if (seq.length > 0) return (seq.length, expr)
      }
    }
    expr match {
      case b: ScBlockExpr =>
        if (b.statements != 1) (-1, expr) else if (b.lastExpr == None) (-1, expr) else isAnonymousExpression(b.lastExpr.get)
      case p: ScParenthesisedExpr => p.expr match {case Some(x) => isAnonymousExpression(x) case _ => (-1, expr)}
      case f: ScFunctionExpr =>  (f.parameters.length, expr)
      case _ => (-1, expr)
    }
  }

  def getModule(element: PsiElement): Module = {
    var index: ProjectFileIndex = ProjectRootManager.getInstance(element.getProject).getFileIndex
    return index.getModuleForFile(element.getContainingFile.getVirtualFile)
  }

  def collectImplicitObjects(tp: ScType, place: PsiElement): Seq[ScObject] = {
    def collectParts(tp: ScType, place: PsiElement): Seq[PsiClass] = {
      tp match {
        case ScCompoundType(comps, _, _, _) => {
          comps.flatMap(collectParts(_, place))
        }
        case p@ScParameterizedType(des, args) => {
          (ScType.extractClass(p) match {
            case Some(pair) => Seq(pair)
            case _ => Seq.empty
          }) ++ args.flatMap(collectParts(_, place))
        }
        case j: JavaArrayType => {
          val parameterizedType = j.getParameterizedType(place.getProject, place.getResolveScope)
          collectParts(parameterizedType.getOrElse(return Seq.empty), place)
        }
        case f@ScFunctionType(retType, params) => {
          (ScType.extractClass(tp) match {
            case Some(pair) => Seq(pair)
            case _ => Seq.empty
          }) ++ params.flatMap(collectParts(_, place)) ++ collectParts(retType, place)
        }
        case f@ScTupleType(params) => {
          (ScType.extractClass(tp) match {
            case Some(pair) => Seq(pair)
            case _ => Seq.empty
          }) ++ params.flatMap(collectParts(_, place))
        }
        case proj@ScProjectionType(projected, _, _) => {
          collectParts(projected, place) ++ (ScType.extractClass(tp) match {
            case Some(pair) => Seq(pair)
            case _ => Seq.empty
          })
        }
        case _=> {
          ScType.extractClass(tp) match {
            case Some(pair) =>
              val packObjects = pair.contexts.flatMap {
                case x: ScPackageLike =>
                  x.findPackageObject(place.getResolveScope).toIterator
                case _ => Iterator()
              }
              Seq(pair) ++ packObjects
            case _ => Seq.empty
          }
        }
      }
    }
    val parts: scala.Seq[PsiClass] = collectParts(tp, place)
    val res: HashSet[ScObject] = new HashSet
    for (part <- parts) {
      for (tp <- MixinNodes.linearization(part, collection.immutable.HashSet.empty)) {
        ScType.extractClass(tp) match {
          case Some(obj: ScObject) => res += obj
          case Some(clazz: PsiClass) => {
            getCompanionModule(clazz) match {
              case Some(obj: ScObject) => res += obj
              case _ => //do nothing
            }
          }
          case _ => //do nothing
        }
      }
    }
    res.toSeq
  }

  def getTypesStream(elems: Seq[PsiParameter]): Stream[ScType] = {
    // TODO Why use such a strange way to construct a stream? Is this here for performance reasons?
    //      Consider elems.toStream.map { case x: ScType => ...; case y => }
    if (elems.isEmpty) return Stream.empty
    new Stream[ScType] {
      override def head: ScType = elems.head match {
        case scp : ScParameter => scp.getType(TypingContext.empty).getOrElse(Nothing)
        case p =>
          val treatJavaObjectAsAny = p.parents.findByType(classOf[PsiClass]) match {
            case Some(cls) if cls.getQualifiedName == "java.lang.Object" => true // See SCL-3036
            case _ => false
          }
          ScType.create(p.getType, p.getProject, paramTopLevel = true, treatJavaObjectAsAny = treatJavaObjectAsAny)
      }

      override def isEmpty: Boolean = false

      private[this] var tlVal: Stream[ScType] = null
      def tailDefined = tlVal ne null

      override def tail: Stream[ScType] = {
        if (!tailDefined) tlVal = getTypesStream(elems.tail)
        tlVal
      }
    }
  }

  /**
   *  This method doesn't collect things like
   *  val a: Type = b //after implicit convesion
   *  Main task for this method to collect imports used in 'for' statemnts.
   */
  def getExprImports(z: ScExpression): Set[ImportUsed] = {
    var res: Set[ImportUsed] = Set.empty
    val visitor = new ScalaRecursiveElementVisitor {
      override def visitExpression(expr: ScExpression) = {
        expr match {
          case f: ScForStatement => {
            f.getDesugarisedExpr match {
              case Some(e) => res = res ++ getExprImports(e)
              case _ =>
            }
          }
          case ref: ScReferenceExpression => {
            for (rr <- ref.multiResolve(false) if rr.isInstanceOf[ScalaResolveResult]) {
              res = res ++ rr.asInstanceOf[ScalaResolveResult].importsUsed
            }
            super.visitExpression(expr)
          }
          case _ => {
            super.visitExpression(expr)
          }
        }
      }
    }
    z.accept(visitor)
    res
  }

  def getPsiElementId(elem: PsiElement): String = {
    if (elem == null || !elem.isValid) "NotValidElement"
    elem match {
      case tp: ScTypeParam => {
        " in:" + (if (elem.getContainingFile != null) elem.getContainingFile.getName else "NoFile") + ":" +
            tp.getOffsetInFile
      }
      case p: PsiTypeParameter => " in: Java" //Two parameters from Java can't be used with same name in same place
      case _ => {
        " in:" + (if (elem.getContainingFile != null)elem.getContainingFile.getName else "NoFile") + ":" +
              (if (elem.getTextRange != null) elem.getTextRange.getStartOffset else "NoRange")
      }
    }
  }

  def getSettings(project: Project): ScalaCodeStyleSettings = {
    CodeStyleSettingsManager.getSettings(project).getCustomSettings(classOf[ScalaCodeStyleSettings])
  }

  def undefineSubstitutor(typeParams: Seq[TypeParameter]): ScSubstitutor = {
    typeParams.foldLeft(ScSubstitutor.empty) {
      (subst: ScSubstitutor, tp: TypeParameter) =>
        subst.bindT((tp.name, ScalaPsiUtil.getPsiElementId(tp.ptp)),
          new ScUndefinedType(new ScTypeParameterType(tp.ptp, ScSubstitutor.empty)))
    }
  }
  def localTypeInference(retType: ScType, params: Seq[Parameter], exprs: Seq[Expression],
                                 typeParams: Seq[TypeParameter],
                                 subst: ScSubstitutor = ScSubstitutor.empty,
                                 shouldUndefineParameters: Boolean = true): ScTypePolymorphicType = {
    localTypeInferenceWithApplicability(retType, params, exprs, typeParams, subst, shouldUndefineParameters)._1
  }

  def polymorphicTypeSubstitutorMissedEmptyParams(typeParameters: Seq[TypeParameter]): ScSubstitutor =
    new ScSubstitutor(new HashMap[(String, String), ScType] ++ Map(typeParameters.flatMap(tp =>
      if (tp.upperType.equiv(Any))
        Seq(((tp.name, ScalaPsiUtil.getPsiElementId(tp.ptp)), tp.lowerType))
      else if (tp.lowerType.equiv(Nothing))
        Seq(((tp.name, ScalaPsiUtil.getPsiElementId(tp.ptp)), tp.upperType))
      else Seq.empty
    ) : _*),  Map.empty, None)


  def localTypeInferenceWithApplicability(retType: ScType, params: Seq[Parameter], exprs: Seq[Expression],
                                          typeParams: Seq[TypeParameter],
                                          subst: ScSubstitutor = ScSubstitutor.empty,
                                          shouldUndefineParameters: Boolean = true): (ScTypePolymorphicType, Seq[ApplicabilityProblem]) = {
    val (tp, problems, _) = localTypeInferenceWithApplicabilityExt(retType, params, exprs, typeParams, subst, shouldUndefineParameters)
    (tp, problems)
  }

  def localTypeInferenceWithApplicabilityExt(retType: ScType, params: Seq[Parameter], exprs: Seq[Expression],
                                             typeParams: Seq[TypeParameter],
                                             subst: ScSubstitutor = ScSubstitutor.empty,
                                             shouldUndefineParameters: Boolean = true): (ScTypePolymorphicType, Seq[ApplicabilityProblem], Seq[(Parameter, ScExpression)]) = {
    // See SCL-3052. TODO: SCL-3058
    // This corresponds to use of `isCompatible` in `Infer#methTypeArgs` in scalac, where `isCompatible` uses `weak_<:<`
    val checkWeak = true

    val s: ScSubstitutor = if (shouldUndefineParameters) undefineSubstitutor(typeParams) else ScSubstitutor.empty
    val paramsWithUndefTypes = params.map(p => p.copy(paramType = s.subst(p.paramType)))
    val c = Compatibility.checkConformanceExt(true, paramsWithUndefTypes, exprs, true, false)
    val tpe = if (c.problems.isEmpty) {
      val un: ScUndefinedSubstitutor = c.undefSubst
      val prevInfoSubst = polymorphicTypeSubstitutorMissedEmptyParams(typeParams) followed subst
      ScTypePolymorphicType(retType, typeParams.map(tp => {
        var lower = tp.lowerType
        var upper = tp.upperType
        for ((name, addLower) <- un.lowerMap if name == (tp.name, ScalaPsiUtil.getPsiElementId(tp.ptp))) {
          lower = Bounds.lub(lower, prevInfoSubst.subst(addLower), checkWeak = checkWeak)
        }
        for ((name, addUpperSeq) <- un.upperMap if name == (tp.name, ScalaPsiUtil.getPsiElementId(tp.ptp)); addUpper <- addUpperSeq) {
          upper = Bounds.glb(upper, prevInfoSubst.subst(addUpper), checkWeak = checkWeak)
        }
        TypeParameter(tp.name, lower, upper, tp.ptp)
      }))
    } else {
      ScTypePolymorphicType(retType, typeParams)
    }
    (tpe, c.problems, c.matchedArgs)
  }

  def getElementsRange(start: PsiElement, end: PsiElement): Seq[PsiElement] = {
    val file = start.getContainingFile
    if (file == null || file != end.getContainingFile) return Nil

    val commonParent = PsiTreeUtil.findCommonParent(start, end)
    val startOffset = start.getTextRange.getStartOffset
    val endOffset = end.getTextRange.getEndOffset
    if (commonParent.getTextRange.getStartOffset == startOffset &&
      commonParent.getTextRange.getEndOffset == endOffset) {
      var parent = commonParent.getParent
      var prev = commonParent
      while (parent.getTextRange.equalsToRange(prev.getTextRange.getStartOffset, prev.getTextRange.getEndOffset)) {
        prev = parent
        parent = parent.getParent
      }
      return Seq(prev)
    }
    val buffer = new ArrayBuffer[PsiElement]
    var child = commonParent.getNode.getFirstChildNode
    var writeBuffer = false
    while (child != null) {
      if (child.getTextRange.getStartOffset == startOffset) {
        writeBuffer = true
      }
      if (writeBuffer) buffer.append(child.getPsi)
      if (child.getTextRange.getEndOffset >= endOffset) {
        writeBuffer = false
      }
      child = child.getTreeNext
    }
    return buffer.toSeq
  }

  def getParents(elem: PsiElement, topLevel: PsiElement): List[PsiElement] = {
    def inner(parent: PsiElement, k: List[PsiElement] => List[PsiElement]): List[PsiElement] = {
      if (parent != topLevel && parent != null)
        inner(parent.getParent, {l => parent :: k(l)})
      else k(Nil)
    }
    inner(elem, (l: List[PsiElement]) => l)
  }

  //todo: always returns true?!
  def shouldCreateStub(elem: PsiElement): Boolean = {
    elem match {
      case _: ScAnnotation | _: ScAnnotations | _: ScFunction | _: ScTypeAlias | _: ScAccessModifier |
              _: ScModifierList | _: ScVariable | _: ScValue | _: ScParameter | _: ScParameterClause |
              _: ScParameters | _: ScTypeParamClause | _: ScTypeParam | _: ScImportExpr |
              _: ScImportSelector | _: ScImportSelectors | _: ScPatternList | _: ScReferencePattern => return shouldCreateStub(elem.getParent)
      /*case _: ScalaFile | _: ScPrimaryConstructor | _: ScSelfTypeElement | _: ScEarlyDefinitions |
              _: ScPackageStatement | _: ScPackaging | _: ScTemplateParents | _: ScTemplateBody |
              _: ScExtendsBlock | _: ScTypeDefinition => return true*/
      case _ => return true
    }
  }

  def getPrevStubOrPsiElement(elem: PsiElement): PsiElement = {
    def workWithStub(stub: StubElement[_ <: PsiElement]): PsiElement = {
      val parent = stub.getParentStub
      val children = parent.getChildrenStubs
      val index = children.indexOf(stub)
      if (index == -1) {
        elem.getPrevSibling
      } else if (index == 0) {
        null
      } else {
        children.get(index - 1).getPsi
      }
    }
    elem match {
      case st: ScalaStubBasedElementImpl[_] if st.getStub != null => {
        val stub = st.getStub
        workWithStub(stub)
      }
      case file: PsiFileImpl if file.getStub != null => {
        val stub = file.getStub
        workWithStub(stub)
      }
      case _ => elem.getPrevSibling
    }
  }

  def isLValue(elem: PsiElement) = elem match {
    case e: ScExpression => e.getParent match {
      case as: ScAssignStmt => as.getLExpression eq e
      case _ => false
    }
    case _ => false
  }

  def getNextStubOrPsiElement(elem: PsiElement): PsiElement = {
    elem match {
      case st: ScalaStubBasedElementImpl[_] if st.getStub != null => {
        val stub = st.getStub
        val parent = stub.getParentStub
        val children = parent.getChildrenStubs
        val index = children.indexOf(stub)
        if (index == -1) {
          elem.getNextSibling
        } else if (index >= children.size - 1) {
          null
        } else {
          children.get(index + 1).getPsi
        }
      }
      case _ => elem.getNextSibling
    }
  }

  def extractReturnType(tp: ScType): Option[ScType] = {
    tp match {
      case ScFunctionType(retType, _) => Some(retType)
      case ScParameterizedType(des, args) => {
        ScType.extractClass(des) match {
          case Some(clazz) if clazz.getQualifiedName.startsWith("scala.Function") => Some(args(args.length - 1))
          case _ => None
        }
      }
      case _ => None
    }
  }

  def getPlaceTd(placer: PsiElement): ScTemplateDefinition = {
    val td = PsiTreeUtil.getContextOfType(placer, true, classOf[ScTemplateDefinition])
    if (td == null) return null
    val res = td.extendsBlock.templateParents match {
      case Some(parents) => {
        if (PsiTreeUtil.isContextAncestor(parents, placer, true)) getPlaceTd(td)
        else td
      }
      case _ => td
    }
    res
  }

  def isPlaceTdAncestor(td: ScTemplateDefinition, placer: PsiElement): Boolean = {
    val newTd = getPlaceTd(placer)
    if (newTd == null) return false
    if (newTd == td) return true
    isPlaceTdAncestor(td, newTd)
  }

  def typesCallSubstitutor(tp: Seq[(String, String)], typeArgs: Seq[ScType]): ScSubstitutor = {
    val map = new collection.mutable.HashMap[(String, String), ScType]
    for (i <- 0 to math.min(tp.length, typeArgs.length) - 1) {
      map += Tuple(tp(i), typeArgs(i))
    }
    new ScSubstitutor(Map(map.toSeq: _*), Map.empty, None)
  }

  def genericCallSubstitutor(tp: Seq[(String, String)], typeArgs: Seq[ScTypeElement]): ScSubstitutor = {
    val map = new collection.mutable.HashMap[(String, String), ScType]
    for (i <- 0 to Math.min(tp.length, typeArgs.length) - 1) {
      map += Tuple(tp(i), typeArgs(i).getType(TypingContext.empty).getOrElse(Any))
    }
    new ScSubstitutor(Map(map.toSeq: _*), Map.empty, None)
  }

  def genericCallSubstitutor(tp: Seq[(String, String)], gen: ScGenericCall): ScSubstitutor = {
    val typeArgs: Seq[ScTypeElement] = gen.arguments
    genericCallSubstitutor(tp, typeArgs)
  }

  def namedElementSig(x: PsiNamedElement): Signature = new Signature(x.getName, Stream.empty, 0, ScSubstitutor.empty)

  def superValsSignatures(x: PsiNamedElement): Seq[FullSignature] = {
    val empty = Seq.empty 
    val typed = x match {case x: ScTypedDefinition => x case _ => return empty}
    val clazz: ScTemplateDefinition = nameContext(typed) match {
      case e @ (_: ScValue | _: ScVariable) if e.getParent.isInstanceOf[ScTemplateBody] => e.asInstanceOf[ScMember].getContainingClass
      case e @ (_: ScObject) if e.getParent.isInstanceOf[ScTemplateBody] => e.containingClass.orNull // todo unify these two cases.
      case _ => return empty
    }
    val s = new FullSignature(namedElementSig(x), new Suspension(() => typed.getType(TypingContext.empty).getOrElse(Any)),
      x.asInstanceOf[NavigatablePsiElement], Some(clazz))
    val sigs = TypeDefinitionMembers.getSignatures(clazz)
    val t = (sigs.get(s): @unchecked) match {
      //partial match
      case Some(x) => x.supers.map {_.info}
      case None => throw new RuntimeException("internal error: could not find signature matching: %s\namong: %s".format(s, sigs.mkString("\n")))
    }
    return t
  }

  def superTypeMembers(element: PsiNamedElement): Seq[PsiNamedElement] = {
    val empty = Seq.empty
    val clazz: ScTemplateDefinition = nameContext(element) match {
      case e @ (_: ScTypeAlias | _: ScTrait | _: ScClass) if e.getParent.isInstanceOf[ScTemplateBody] => e.asInstanceOf[ScMember].getContainingClass
      case _ => return empty
    }
    val sigs = TypeDefinitionMembers.getTypes(clazz)
    val t = (sigs.get(element): @unchecked) match {
      //partial match
      case Some(x) => x.supers.map {_.info}
    }
    return t
  }

  def nameContext(x: PsiNamedElement): PsiElement = {
    var parent = x.getParent
    def isAppropriatePsiElement(x: PsiElement): Boolean = {
      x match {
        case _: ScValue | _: ScVariable | _: ScTypeAlias | _: ScParameter | _: PsiMethod | _: PsiField |
                _: ScCaseClause | _: PsiClass | _: PsiPackage | _: ScGenerator | _: ScEnumerator | _: ScObject => true
        case _ => false
      }
    }
    if (isAppropriatePsiElement(x)) return x
    while (parent != null && !isAppropriatePsiElement(parent)) parent = parent.getParent
    return parent
  }

  def getEmptyModifierList(manager: PsiManager): PsiModifierList =
    new LightModifierList(manager, ScalaFileType.SCALA_LANGUAGE)

  def adjustTypes(element: PsiElement): Unit = {
    for (child <- element.getChildren) {
      child match {
        case x: ScStableCodeReferenceElement => x.resolve match {
          case clazz: PsiClass =>
            x.replace(ScalaPsiElementFactory.createReferenceFromText(clazz.getName, clazz.getManager)).
                    asInstanceOf[ScStableCodeReferenceElement].bindToElement(clazz)
          case m: ScTypeAlias if m.getContainingClass != null && (
                  m.getContainingClass.getQualifiedName == "scala.Predef" ||
                  m.getContainingClass.getQualifiedName == "scala") => {
            x.replace(ScalaPsiElementFactory.createReferenceFromText(m.getName, m.getManager)).
                    asInstanceOf[ScStableCodeReferenceElement].bindToElement(m)
          }
          case _ => adjustTypes(child)
        }
        case _ => adjustTypes(child)
      }
    }
  }

  def getMethodPresentableText(method: PsiMethod): String = {
    val buffer = new StringBuffer("")
    method match {
      case method: ScFunction => {
        return ScalaElementPresentation.getMethodPresentableText(method, false)
      }
      case _ => {
        val PARAM_OPTIONS: Int = PsiFormatUtilBase.SHOW_NAME | PsiFormatUtilBase.SHOW_TYPE | PsiFormatUtilBase.TYPE_AFTER
        return PsiFormatUtil.formatMethod(method, PsiSubstitutor.EMPTY,
          PARAM_OPTIONS | PsiFormatUtilBase.SHOW_PARAMETERS, PARAM_OPTIONS)
      }
    }
  }

  def getModifiersPresentableText(modifiers: ScModifierList): String = {
    if (modifiers == null) return ""
    val buffer = new StringBuilder("")
    modifiers match {
      case st: StubBasedPsiElement[_] if st.getStub != null =>
        for (modifier <- st.getStub.asInstanceOf[ScModifiersStub].getModifiers) buffer.append(modifier + " ")
      case _ =>
        for (modifier <- modifiers.getNode.getChildren(null) if !isLineTerminator(modifier.getPsi)) buffer.append(modifier.getText + " ")
    }
    return buffer.toString
  }

  def isLineTerminator(element: PsiElement): Boolean = {
    element match {
      case _: PsiWhiteSpace if element.getText.indexOf('\n') != -1 => true
      case _ => false
    }
  }

  def getApplyMethods(clazz: PsiClass): Seq[PhysicalSignature] = {
    (for ((n: PhysicalSignature, _) <- TypeDefinitionMembers.getMethods(clazz)
          if n.method.getName == "apply" &&
                  (clazz.isInstanceOf[ScObject] || !n.method.hasModifierProperty("static"))) yield n).toSeq
  }

  def getUnapplyMethods(clazz: PsiClass): Seq[PhysicalSignature] = {
    (for ((n: PhysicalSignature, _) <- TypeDefinitionMembers.getMethods(clazz)
          if (n.method.getName == "unapply" || n.method.getName == "unapplySeq") &&
                  (clazz.isInstanceOf[ScObject] || n.method.hasModifierProperty("static"))) yield n).toSeq ++
    (clazz match {
      case c: ScObject => c.objectSyntheticMembers.filter(s => s.getName == "unapply" || s.getName == "unapplySeq").
              map(new PhysicalSignature(_, ScSubstitutor.empty))
      case _ => Seq.empty[PhysicalSignature]
    })
  }

  def getUpdateMethods(clazz: PsiClass): Seq[PhysicalSignature] = {
    (for ((n: PhysicalSignature, _) <- TypeDefinitionMembers.getMethods(clazz)
          if n.method.getName == "update" &&
                  (clazz.isInstanceOf[ScObject] || !n.method.hasModifierProperty("static"))) yield n).toSeq
  }

  /**
   *  For one classOf use PsiTreeUtil.getParenteOfType instead
   */
  def getParentOfType(element: PsiElement, classes: Class[_ <: PsiElement]*): PsiElement = {
    getParentOfType(element, false, classes: _*)
  }

  /**
   * For one classOf use PsiTreeUtil.getParenteOfType instead
   */
  def getParentOfType(element: PsiElement, strict: Boolean, classes: Class[_ <: PsiElement]*): PsiElement = {
    var el: PsiElement = if (!strict) element else {
      if (element == null) return null
      element.getParent
    }
    while (el != null && classes.find(_.isInstance(el)) == None) el = el.getParent
    return el
  }

  /**
   * For one classOf use PsiTreeUtil.getContextOfType instead
   */
  def getContextOfType(element: PsiElement, strict: Boolean, classes: Class[_ <: PsiElement]*): PsiElement = {
    var el: PsiElement = if (!strict) element else {
      if (element == null) return null
      element.getContext
    }
    while (el != null && classes.find(_.isInstance(el)) == None) el = el.getContext
    return el
  }

  def getCompanionModule(clazz: PsiClass): Option[ScTypeDefinition] = {
    getBaseCompanionModule(clazz) match {
      case Some(td) => Some(td)
      case _ =>
        clazz match {
          case x: ScClass if x.isCase => x.fakeCompanionModule
          case _ => None
        }
    }
  }

  def getBaseCompanionModule(clazz: PsiClass): Option[ScTypeDefinition] = {
    if (!clazz.isInstanceOf[ScTypeDefinition]) return None
    val td = clazz.asInstanceOf[ScTypeDefinition]
    val name: String = td.getName
    val scope: PsiElement = td.getContext
    val arrayOfElements: Array[PsiElement] = scope match {
      case stub: StubBasedPsiElement[_] if stub.getStub != null =>
        stub.getStub.getChildrenByType(TokenSets.TYPE_DEFINITIONS_SET, arrayFactory[PsiElement])
      case file: PsiFileImpl if file.getStub != null => file.getStub.getChildrenByType(TokenSets.TYPE_DEFINITIONS_SET, arrayFactory[PsiElement])
      case _ => scope.getChildren
    }
    td match {
      case _: ScClass | _: ScTrait => {
        arrayOfElements.map((child: PsiElement) =>
          child match {
            case td: ScTypeDefinition => td
            case _ => null: ScTypeDefinition
          }).find((child: ScTypeDefinition) =>
          child.isInstanceOf[ScObject] && child.asInstanceOf[ScObject].getName == name)
      }
      case _: ScObject => {
        arrayOfElements.map((child: PsiElement) =>
          child match {
            case td: ScTypeDefinition => td
            case _ => null: ScTypeDefinition
          }).find((child: ScTypeDefinition) =>
          (child.isInstanceOf[ScClass] || child.isInstanceOf[ScTrait])
                  && child.asInstanceOf[ScTypeDefinition].getName == name)
      }
      case _ => None
    }
  }

  def getPsiSubstitutor(subst: ScSubstitutor, project: Project, scope: GlobalSearchScope): PsiSubstitutor = {
    case class PseudoPsiSubstitutor(substitutor: ScSubstitutor) extends PsiSubstitutor {
      def putAll(parentClass: PsiClass, mappings: Array[PsiType]): PsiSubstitutor = PsiSubstitutor.EMPTY

      def isValid: Boolean = true

      def put(classParameter: PsiTypeParameter, mapping: PsiType): PsiSubstitutor = PsiSubstitutor.EMPTY

      def getSubstitutionMap: java.util.Map[PsiTypeParameter, PsiType] = new java.util.HashMap[PsiTypeParameter, PsiType]()

      def substitute(`type` : PsiType): PsiType = {
        ScType.toPsi(substitutor.subst(ScType.create(`type`, project, scope)), project, scope)
      }

      def substitute(typeParameter: PsiTypeParameter): PsiType = {
        ScType.toPsi(substitutor.subst(new ScTypeParameterType(typeParameter, substitutor)),
          project, scope)
      }

      def putAll(another: PsiSubstitutor): PsiSubstitutor = PsiSubstitutor.EMPTY

      def substituteWithBoundsPromotion(typeParameter: PsiTypeParameter): PsiType = substitute(typeParameter)
    }

    PseudoPsiSubstitutor(subst)
  }

  //todo: rewrite this!
  def needParentheses(from: ScExpression, expr: ScExpression): Boolean = {
    val parent = from.getParent
    (parent, expr) match { //true only for other cases
      case _ if !parent.isInstanceOf[ScExpression] => false
      case (_: ScTuple, _) => false
      case (_: ScReferenceExpression, _: ScBlockExpr) => false
      case (_: ScReferenceExpression, _: ScNewTemplateDefinition) if expr.getText.endsWith(")") ||
              expr.getText.endsWith("}") || expr.getText.endsWith("]") => false
      case (_: ScReferenceExpression, _: ScReferenceExpression) => false
      case (_: ScReferenceExpression, _: ScGenericCall) => false
      case (_: ScReferenceExpression, _: ScMethodCall) => false
      case (_: ScReferenceExpression, _: ScLiteral) => false
      case (_: ScReferenceExpression, _) if expr.getText == "_" => false
      case (_: ScReferenceExpression, _: ScTuple) => false
      case (_: ScReferenceExpression, _: ScXmlExpr) => false
      case (_: ScReferenceExpression, _) => true
      case (_: ScGenericCall, _: ScReferenceExpression) => false
      case (_: ScGenericCall, _: ScMethodCall) => false
      case (_: ScGenericCall, _: ScLiteral) => false
      case (_: ScGenericCall, _: ScTuple) => false
      case (_: ScGenericCall, _: ScXmlExpr) => false
      case (_: ScGenericCall, _) if expr.getText == "_" => false
      case (_: ScGenericCall, _: ScBlockExpr) => false
      case (_: ScGenericCall, _) => true
      case (_: ScMethodCall, _: ScReferenceExpression) => false
      case (_: ScMethodCall, _: ScMethodCall) => false
      case (_: ScMethodCall, _: ScGenericCall) => false
      case (_: ScMethodCall, _: ScLiteral) => false
      case (_: ScMethodCall, _) if expr.getText == "_" => false
      case (_: ScMethodCall, _: ScXmlExpr) => false
      case (_: ScMethodCall, _: ScTuple) => false
      case (_: ScMethodCall, _) => true
      case (_: ScUnderscoreSection, _: ScReferenceExpression) => false
      case (_: ScUnderscoreSection, _: ScMethodCall) => false
      case (_: ScUnderscoreSection, _: ScGenericCall) => false
      case (_: ScUnderscoreSection, _: ScLiteral) => false
      case (_: ScUnderscoreSection, _) if expr.getText == "_" => false
      case (_: ScUnderscoreSection, _: ScXmlExpr) => false
      case (_: ScUnderscoreSection, _: ScTuple) => false
      case (_: ScUnderscoreSection, _) => true
      case (_: ScBlock, _) => false
      case (_: ScPrefixExpr, _: ScBlockExpr) => false
      case (_: ScPrefixExpr, _: ScNewTemplateDefinition) => false
      case (_: ScPrefixExpr, _: ScUnderscoreSection) => false
      case (_: ScPrefixExpr, _: ScReferenceExpression) => false
      case (_: ScPrefixExpr, _: ScGenericCall) => false
      case (_: ScPrefixExpr, _: ScMethodCall) => false
      case (_: ScPrefixExpr, _: ScLiteral) => false
      case (_: ScPrefixExpr, _) if expr.getText == "_" => false
      case (_: ScPrefixExpr, _: ScTuple) => false
      case (_: ScPrefixExpr, _: ScXmlExpr) => false
      case (_: ScPrefixExpr, _) => true
      case (_: ScInfixExpr, _: ScBlockExpr) => false
      case (_: ScInfixExpr, _: ScNewTemplateDefinition) => false
      case (_: ScInfixExpr, _: ScUnderscoreSection) => false
      case (_: ScInfixExpr, _: ScReferenceExpression) => false
      case (_: ScInfixExpr, _: ScGenericCall) => false
      case (_: ScInfixExpr, _: ScMethodCall) => false
      case (_: ScInfixExpr, _: ScLiteral) => false
      case (_: ScInfixExpr, _) if expr.getText == "_" => false
      case (_: ScInfixExpr, _: ScTuple) => false
      case (_: ScInfixExpr, _: ScXmlExpr) => false
      case (_: ScInfixExpr, _: ScPrefixExpr) => false
      case (par: ScInfixExpr, child: ScInfixExpr) => {
        import ParserUtils._
        import InfixExpr._
        if (par.lOp == from) {
          val lid = par.operation.getText
          val rid = child.operation.getText
          if (priority(lid) < priority(rid)) true
          else if (priority(rid) < priority(lid)) false
          else if (associate(lid) != associate(rid)) true
          else if (associate(lid) == -1) true
          else false
        }
        else {
          val lid = child.operation.getText
          val rid = par.operation.getText
          if (priority(lid) < priority(rid)) false
          else if (priority(rid) < priority(lid)) true
          else if (associate(lid) != associate(rid)) true
          else if (associate(lid) == -1) false
          else true
        }
      }
      case (_: ScInfixExpr, _) => true
      case (_: ScPostfixExpr, _: ScBlockExpr) => false
      case (_: ScPostfixExpr, _: ScNewTemplateDefinition) => false
      case (_: ScPostfixExpr, _: ScUnderscoreSection) => false
      case (_: ScPostfixExpr, _: ScReferenceExpression) => false
      case (_: ScPostfixExpr, _: ScGenericCall) => false
      case (_: ScPostfixExpr, _: ScMethodCall) => false
      case (_: ScPostfixExpr, _: ScLiteral) => false
      case (_: ScPostfixExpr, _) if expr.getText == "_" => false
      case (_: ScPostfixExpr, _: ScTuple) => false
      case (_: ScPostfixExpr, _: ScXmlExpr) => false
      case (_: ScPostfixExpr, _: ScPrefixExpr) => false
      case (_: ScPostfixExpr, _) => true
      case (_: ScTypedStmt, _: ScBlockExpr) => false
      case (_: ScTypedStmt, _: ScNewTemplateDefinition) => false
      case (_: ScTypedStmt, _: ScUnderscoreSection) => false
      case (_: ScTypedStmt, _: ScReferenceExpression) => false
      case (_: ScTypedStmt, _: ScGenericCall) => false
      case (_: ScTypedStmt, _: ScMethodCall) => false
      case (_: ScTypedStmt, _: ScLiteral) => false
      case (_: ScTypedStmt, _) if expr.getText == "_" => false
      case (_: ScTypedStmt, _: ScTuple) => false
      case (_: ScTypedStmt, _: ScXmlExpr) => false
      case (_: ScTypedStmt, _: ScPrefixExpr) => false
      case (_: ScTypedStmt, _: ScInfixExpr) => false
      case (_: ScTypedStmt, _: ScPostfixExpr) => false
      case (_: ScTypedStmt, _) => true
      case (_: ScMatchStmt, _: ScBlockExpr) => false
      case (_: ScMatchStmt, _: ScNewTemplateDefinition) => false
      case (_: ScMatchStmt, _: ScUnderscoreSection) => false
      case (_: ScMatchStmt, _: ScReferenceExpression) => false
      case (_: ScMatchStmt, _: ScGenericCall) => false
      case (_: ScMatchStmt, _: ScMethodCall) => false
      case (_: ScMatchStmt, _: ScLiteral) => false
      case (_: ScMatchStmt, _) if expr.getText == "_" => false
      case (_: ScMatchStmt, _: ScTuple) => false
      case (_: ScMatchStmt, _: ScXmlExpr) => false
      case (_: ScMatchStmt, _: ScPrefixExpr) => false
      case (_: ScMatchStmt, _: ScInfixExpr) => false
      case (_: ScMatchStmt, _: ScPostfixExpr) => false
      case (_: ScMatchStmt, _) => true
      case _ => false
    }
  }

  def isScope(element: PsiElement): Boolean = element match {
    case _: ScalaFile | _: ScBlock | _: ScTemplateBody | _: ScPackageContainer | _: ScParameters |
            _: ScTypeParamClause | _: ScCaseClause | _: ScForStatement | _: ScExistentialClause |
            _: ScEarlyDefinitions | _: ScRefinement => true
    case e: ScPatternDefinition if e.getContext.isInstanceOf[ScCaseClause] => true // {case a => val a = 1}
    case _ => false
  }

  def shouldChangeModificationCount(place: PsiElement): Boolean = {
    var parent = place.getParent
    while (parent != null) {
      parent match {
        case f: ScFunction => f.returnTypeElement match {
          case Some(ret) => return false
          case None => if (!f.hasAssign) return false
        }
        case t: PsiClass => return true
        case bl: ScBlockExprImpl => return bl.shouldChangeModificationCount(null)
        case _ =>
      }
      parent = parent.getParent
    }
    return false
  }

  def stringValueOf(e: PsiLiteral): Option[String] = e.getValue.toOption.flatMap(_.asOptionOf[String])

  def readAttribute(annotation: PsiAnnotation, name: String): Option[String] = {
    annotation.findAttributeValue(name) match {
      case literal: PsiLiteral => stringValueOf(literal)
      case element: ScReferenceElement => element.getReference.toOption
              .flatMap(_.resolve.asOptionOf[ScBindingPattern])
              .flatMap(_.getParent.asOptionOf[ScPatternList])
              .filter(_.allPatternsSimple)
              .flatMap(_.getParent.asOptionOf[ScPatternDefinition])
              .flatMap(_.expr.asOptionOf[PsiLiteral])
              .flatMap(stringValueOf(_))
      case _ => None
    }
  }

  def arrayFactory[T <: AnyRef : Manifest] = new ArrayFactory[T] {
    // See http://lampsvn.epfl.ch/trac/scala/ticket/4390
    def create(count: Int): Array[T with Object] = Array.ofDim[T](count).asInstanceOf[Array[T with Object]]
  }

  /**
   * Finds the n-th parameter from the primiary constructor of `cls`
   */
  def nthConstructorParam(cls: ScClass, n: Int): Option[ScParameter] = cls.constructor match {
    case Some(x: ScPrimaryConstructor) =>
      val clauses = x.parameterList.clauses
      if (clauses.length == 0) None
      else {
        val params = clauses(0).parameters
        if (params.length > n)
          Some(params(n))
        else
          None
      }
    case _ => None
  }

  /**
   * @return Some(parameter) if the expression is an argument expression that can be resolved to a corresponding
   *         parameter; None otherwise.
   */
  def parameterOf(exp: ScExpression): Option[Parameter] = {
    exp match {
      case assignment: ScAssignStmt =>
        assignment.getLExpression match {
          case ref: ScReferenceExpression =>
            ref.resolve().asOptionOf[ScParameter].map(p => new Parameter(p))
          case _ => None
        }
      case _ =>
        exp.getParent match {
          case ie: ScInfixExpr if exp == (if (ie.isLeftAssoc) ie.lOp else ie.rOp) =>
            ie.operation match {
              case Resolved(f: ScFunction, _) => f.parameters.headOption.map(p => new Parameter(p))
              case _ => None
            }
          case args: ScArgumentExprList =>
            args.getParent match {
              case constructor: ScConstructor =>
                constructor.reference
                        .flatMap(_.resolve().asOptionOf[ScPrimaryConstructor]) // TODO secondary constructors
                        .flatMap(_.parameters.lift(args.exprs.indexOf(exp)))   // TODO secondary parameter lists
                        .map(p => new Parameter(p))
              case _ => args.matchedParameters.getOrElse(Map.empty).get(exp)
            }
          case _ => None
        }
    }
  }

  /**
   * If `param` is a synthetic parameter with a corresponding real parameter, return Some(realParameter), otherwise None
   */
  def parameterForSyntheticParameter(param: ScParameter): Option[ScParameter] = {
    val fun = PsiTreeUtil.getParentOfType(param, classOf[ScFunction], true)

    def paramFromConstructor(td: ScClass) = td.constructor match {
      case Some(constr) => constr.parameters.find(p => p.name == param.name) // TODO multiple parameter sections.
      case _ => None
    }

    if (fun == null) {
      None
    } else if (fun.isSyntheticCopy) {
      fun.getContainingClass match {
        case td: ScClass if td.isCase => paramFromConstructor(td)
        case _ => None
      }
    } else if (fun.isSyntheticApply) {
      getCompanionModule(fun.getContainingClass) match {
        case Some(td: ScClass) if td.isCase => paramFromConstructor(td)
        case _ => None
      }
    } else None
  }

  def isReadonly(e: PsiElement): Boolean = {
    if(e.isInstanceOf[ScClassParameter]) {
      return e.asInstanceOf[ScClassParameter].isVal
    }

    if(e.isInstanceOf[ScParameter]) {
      return true
    }

    val parent = e.getParent

    if(parent.isInstanceOf[ScGenerator] ||
            parent.isInstanceOf[ScEnumerator] ||
            parent.isInstanceOf[ScCaseClause]) {
      return true
    }

    e.parentsInFile.takeWhile(!_.isScope).findByType(classOf[ScPatternDefinition]).isDefined
  }

}
