package org.jetbrains.plugins.scala.lang.psi.types

/**
* @author ilyas
*/

import api.toplevel.typedef._
import api.statements.{ScTypeAliasDefinition, ScTypeAlias}
import api.toplevel.ScTypeParametersOwner
import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.{PsiTypeParameterListOwner, JavaPsiFacade, PsiElement, PsiNamedElement}
import resolve.{ResolveProcessor, StdKinds}
import api.statements.params.ScTypeParam
import psi.impl.ScalaPsiManager

case class ScDesignatorType(val element: PsiNamedElement) extends ScType {
  override def equiv(t: ScType) = t match {
    case ScDesignatorType(element1) => element eq element1
    case p : ScProjectionType => p equiv this
    case _ => false
  }
}

import _root_.scala.collection.immutable.{Map, HashMap}
import com.intellij.psi.{PsiTypeParameter, PsiClass}

case class ScParameterizedType(designator : ScType, typeArgs : Array[ScType]) extends ScType {
  def designated = ScType.extractDesignated(designator) match {
    case Some((e, _)) => Some(e)
    case _ => None
  }
  
  val substitutor : ScSubstitutor = {
    val (params, initial) = designator match {
      case ScPolymorphicType(_, args, _, _) => (args, ScSubstitutor.empty)
      case _ => ScType.extractDesignated(designator) match {
        case Some((owner : ScTypeParametersOwner, s)) => (owner.typeParameters.map {tp => ScalaPsiManager.typeVariable(tp)}, s)
        case Some((owner : PsiTypeParameterListOwner, s)) => (owner.getTypeParameters.map {tp => ScalaPsiManager.typeVariable(tp)}, s)
        case _ => (Seq.empty, ScSubstitutor.empty)
      }
    }

    params match {
      case Seq.empty => initial
      case _ => {
        var res = initial
        for (p <- params.toArray zip typeArgs) {
          res = res bindT (p._1.name, p._2)
        }
        res
      }
    }
  }

  override def equiv(t: ScType): Boolean = t match {
    case ScParameterizedType(designator1, typeArgs1) => {
      return designator.equiv(designator1) &&
             typeArgs.equalsWith(typeArgs1) {_ equiv _}
    }
    case fun : ScFunctionType => fun equiv this
    case tuple : ScTupleType => tuple equiv this
    case _ => false
  }
}

object ScParameterizedType {
  def create(c: PsiClass, s : ScSubstitutor) =
    new ScParameterizedType(new ScDesignatorType(c), c.getTypeParameters.map {
      tp => s subst(ScalaPsiManager.typeVariable(tp))
    })
}

abstract case class ScPolymorphicType(name : String, args : List[ScTypeParameterType],
                                     lower : Suspension[ScType], upper : Suspension[ScType]) extends ScType

case class ScTypeConstructorType(alias : ScTypeAliasDefinition, override val args : List[ScTypeParameterType],
                                 aliased : Suspension[ScType])
extends ScPolymorphicType(alias.name, args, aliased, aliased) {
  override def equiv(t: ScType) = t match {
    case tct : ScTypeConstructorType => alias == tct.alias && {
      val s = args.zip(tct.args).foldLeft(ScSubstitutor.empty) {(s, p) => s bindT (p._2.name, p._1)}
      lower.v.equiv(s.subst(tct.lower.v)) && upper.v.equiv(s.subst(tct.upper.v))
    }
    case _ => false
  }

  def this(tad : ScTypeAliasDefinition, s : ScSubstitutor) =
    this(tad, tad.typeParameters.toList.map{new ScTypeParameterType(_, s)},
      new Suspension[ScType]({() => s.subst(tad.aliasedType)}))
}

case class ScTypeAliasType(alias : ScTypeAlias, override val args : List[ScTypeParameterType],
                           override val lower : Suspension[ScType], override val upper : Suspension[ScType])
extends ScPolymorphicType(alias.name, args, lower, upper) {
    override def equiv(t: ScType) = t match {
      case tat : ScTypeAliasType => alias == tat.alias && {
        val s = args.zip(tat.args).foldLeft(ScSubstitutor.empty) {(s, p) => s bindT (p._2.name, p._1)}
        lower.v.equiv(s.subst(tat.lower.v)) && upper.v.equiv(s.subst(tat.upper.v))
      }
      case _ => false
    }

  def this(ta : ScTypeAlias, s : ScSubstitutor) =
    this(ta, ta.typeParameters.toList.map{new ScTypeParameterType(_, s)},
      new Suspension[ScType]({() => s.subst(ta.lowerBound)}),
      new Suspension[ScType]({() => s.subst(ta.upperBound)}))
}

case class ScTypeParameterType(override val name : String, override val args : List[ScTypeParameterType],
                               override val lower : Suspension[ScType], override val upper : Suspension[ScType])
extends ScPolymorphicType(name, args, lower, upper) {
  def this(tp : ScTypeParam, s : ScSubstitutor) =
    this(tp.name, tp.typeParameters.toList.map{new ScTypeParameterType(_, s)},
      new Suspension[ScType]({() => s.subst(tp.lowerBound)}),
      new Suspension[ScType]({() => s.subst(tp.upperBound)}))


  override def equiv(t: ScType) = t match {
    case stp: ScTypeParameterType => lower.v.equiv(stp.lower.v) &&
            upper.v.equiv(stp.upper.v)
    case _ => false
  }
}

case class ScTypeVariable(name : String) extends ScType