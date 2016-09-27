package orc.scala.impl

import scala.reflect.macros.blackbox.Context
import orc.scala.Orc
import scala.concurrent.Future
import scala.annotation.compileTimeOnly
import orc.util.ListExtensions._
import scala.language.experimental.macros

object OrcMacroInternal {
  @compileTimeOnly("[Orclave] ICE: Future value can only be used as Future[T] outside an orclave. An expression was used " +
    "as T. This error was probably caused by a bug in the orclave macro.")
  def futureToBeLifted[T](f: Future[T]): T = {
    orc.scala.Logger.severe(s"futureToBeLifted should never appear in a compiled program. There is a bug in the macro.\n$f")
    throw new AssertionError("futureToBeLifted should never appear in a compiled program. There is a bug in the macro.")
  }
}

/*
 * General principles for macro writing:
 * 
 * - Either be totally typed or totally untyped.
 * - U: Always reconstruct every tree without copying symbols or anything
 * - T: Insert types manual if you have them (setType).
 * - Always set position if possible.
 * - T: Always type check as you go using typedTransform.
 * - T: Process elements in order (since typing has side effects).
 * - T: Remember that trees have types that can be different from the symbol to which they refer.
 */

class OrcMacro(val c: Context) extends OwnerSplicer {
  import c._
  import c.universe._
  //import compat._
  import c.internal._
  import c.internal.decorators._

  object TypeTrees {
    val ScalaExpr = typecheck(tq"_root_.orc.scala.impl.ScalaExpr", TYPEmode)
    val Orc = typecheck(tq"_root_.orc.scala.Orc", TYPEmode)
    val OrcSym = Orc.symbol
    val OrcLowPriorityImplicits = typecheck(tq"_root_.orc.scala.OrcLowPriorityImplicits", TYPEmode)
    val OrcLowPriorityImplicitsSym = OrcLowPriorityImplicits.symbol
    val Future = typecheck(tq"_root_.scala.concurrent.Future", TYPEmode)
    val FutureSym = Future.symbol
    val Graft = typecheck(tq"_root_.orc.scala.Graft", TYPEmode)
    val GraftSym = Graft.symbol
  }

  object Constants {
    import TypeTrees._
    val OrcMacroInternal = q"_root_.orc.scala.impl.OrcMacroInternal"
    val orcInScalaContext = q"_root_.orc.scala.Orc.orcInScalaContext"
    val orcInScalaContextSym = OrcLowPriorityImplicitsSym.info.member(TermName("orcInScalaContext"))
    val futureInScalaContextSym = OrcLowPriorityImplicitsSym.info.member(TermName("futureInScalaContext"))
    val futureInOrcContextSym = OrcLowPriorityImplicitsSym.info.member(TermName("futureInOrcContext"))
    val scalaInOrcContext = q"_root_.orc.scala.Orc.scalaInOrcContext"
    val scalaInOrcContextSym = OrcLowPriorityImplicitsSym.info.member(TermName("scalaInOrcContext"))
    val Orc_mapSym = OrcSym.info.member(TermName("map"))
    val variable = q"_root_.orc.scala.Orc.variable"
    val scalaExpr = q"_root_.orc.scala.Orc.scalaExpr"
    val FutureUtil = reify(orc.scala.impl.FutureUtil).tree
  }


  object Types {
    val Orc = c.typeOf[orc.scala.Orc[Unit]].typeConstructor
    val Future = c.typeOf[scala.concurrent.Future[Unit]].typeConstructor
    val Graft = c.typeOf[orc.scala.Graft[Unit]].typeConstructor
  }

  def println(s: String, pos: Position = enclosingPosition): Unit = c.echo(pos, s)

  sealed trait ValueKind
  case object ScalaValue extends ValueKind
  case object OrcValue extends ValueKind
  case object FutureValue extends ValueKind
  case object GraftValue extends ValueKind

  object ValueKind {
    /** Return the kind of value that matches this type.
      */
    def apply(tpe: Type): ValueKind = {
      import TypeTrees._
      tpe match {
        case TypeRef(_, OrcSym, List(tArg)) =>
          OrcValue
        case TypeRef(_, FutureSym, List(tArg)) =>
          FutureValue
        case TypeRef(_, GraftSym, List(tArg)) =>
          GraftValue
        case _ =>
          ScalaValue
      }
    }
  }

  sealed trait OrclaveGenerated
  object OrclaveGenerated extends OrclaveGenerated

  implicit class TreeAdds(val t: c.Tree) {
    def setGenerated(): t.type = {
      t.updateAttachment(OrclaveGenerated)
      t
    }
  }
  implicit class TypeAdds(val t: c.Type) {
    def withoutKind: Type = {
      import TypeTrees._
      t match {
        case TypeRef(_, OrcSym, List(tArg)) =>
          tArg
        case TypeRef(_, FutureSym, List(tArg)) =>
          tArg
        case TypeRef(_, GraftSym, List(tArg)) =>
          tArg
        case _ =>
          t
      }
    }

    def withKind(k: ValueKind): Type = {
      import TypeTrees._
      k match {
        case OrcValue =>
          appliedType(OrcSym, withoutKind)
        case FutureValue =>
          appliedType(FutureSym, withoutKind)
        case GraftValue =>
          appliedType(GraftSym, withoutKind)
        case ScalaValue =>
          t
      }
    }

    def kind: ValueKind = ValueKind(t)
  }

  /** Transform a Scala tree representing Orc code into an implementation of that Orc.
    *
    * transform should always return a tree that will be typed to Orc[T] for T being the type
    * of the provided tree with any Future or Orc constructor removed.
    *
    */
  case class OrcTransformer() extends ((Tree, TypingTransformApi) => Tree) {
    /* Transformations are documented in Orclave/Transform.txt
     * 
     * They fall into several categories:
     * + Constants and variables
     * + Applications
     * - Graft
     * - Def
     * + Congruence on calls to Orc
     */
    
    // TODO: This uses untypecheck. 
    // While lazy will be disallowed anyway and case classes don't seem useful, match destructors seem important eventually. 

    def buildKindConvertion(sk: ValueKind, tk: ValueKind)(t: Tree): Tree = {
      import Constants._
      (sk, tk) match {
        case (k1, k2) if k1 == k2 => t
        case (ScalaValue, OrcValue) =>
          q"$scalaExpr($t)"
        case (FutureValue, OrcValue) =>
          q"$variable($t)"
        case _ =>
          throw new IllegalArgumentException(s"Illegal conversion requested: from $sk to $tk")
      }
    }

    /** Argument lifting into grafts for the given arguments.
      *
      * The type information is used for correctly selecting kind conversion for
      * arguments and results.
      *
      * @param argss     The argument trees for the function call.
      * @param argTypess The argument types for the function.
      * @param returnType The returned type for the function.
      * @param coreFunc  Given a list of trees of the appropriate kinds build the call. The provided trees will not be typed.
      */
    def buildApply(argss: List[List[Tree]], argTypess: List[List[Type]], returnType: Type, pos: Position)(coreFunc: List[List[Tree]] => Tree): Tree = {
      // TODO: Support mixed argument kinds
      val argKindss = argTypess.innerMap(ValueKind(_))
      val argKind = argKindss.head.head
      assert(argKindss.flatten.forall(_ == argKind))
      val returnKind = ValueKind(returnType)

      def argRelatedNames(kind: String): List[List[TermName]] = argss innerMap { a =>
        val s =
          if (a.symbol != null && a.symbol != NoSymbol)
            a.symbol.name.toString()
          else
            "expr"
        freshName(TermName(s"_${s}_$kind"))
      }
      val graftNames = argRelatedNames("graft")

      val graftValDefs = (argss innerZip graftNames innerZip argTypess).flatten.map { p =>
        val ((arg, name), tpe) = p
        // Usually correct type: tpe.widen.withKind(GraftValue)
        // However this type seems to sometimes contain type variables
        val r = ValDef(Modifiers(Flag.SYNTHETIC), name, TypeTree(), q"${recur(arg)}.graft")
        r.setGenerated()
        r.setPos(arg.pos)
        r
      }

      val futureRefs = graftNames.innerMap { (a: TermName) => q"$a.future" }
      val bodyRefs = graftNames.flatten.map { a => q"$a.body" }

      def convertCore(t: Tree) = buildKindConvertion(returnKind, OrcValue)(t)

      val forceAndCall: Tree = argKind match {
        case FutureValue =>
          convertCore(coreFunc(futureRefs))
        case ScalaValue =>
          futureRefs.flatten match {
            case Seq() =>
              coreFunc(futureRefs)
            case Seq(f) =>
              val forcedNames = argRelatedNames("forced")
              val forcedName = forcedNames.flatten.head
              val futureRef = futureRefs.flatten.head
              q"""
              ${Constants.variable}($futureRef).map((${ValDef(Modifiers(Flag.PARAM), forcedName, TypeTree(), EmptyTree)}) => { 
                ${convertCore(coreFunc(forcedNames.innerMap(a => q"$a")))}
              })
              """
            case toFutures =>
              val forcedNames = argRelatedNames("forced")
              lazy val t = freshName(TermName("_tuple"))
              lazy val tParam = ValDef(Modifiers(Flag.PARAM), t, TypeTree(), EmptyTree)
              lazy val forceBinders = forcedNames.flatten.zipWithIndex.map(p => {
                val (n, i) = p
                val r = ValDef(Modifiers(Flag.SYNTHETIC), n, TypeTree(), q"$t.${TermName(s"_${i + 1}")}")
                r.setGenerated()
                r
              })
              q"""
              ${Constants.variable}(${Constants.FutureUtil}.tuple(..${futureRefs.flatten})).map(($tParam) => { 
                ..$forceBinders;
                ${convertCore(coreFunc(forcedNames.innerMap(a => q"$a")))}
              })
              """
          }
        case OrcValue =>
          error(pos, "Orc[_] argument types are not usable from within an Orclave.")
          coreFunc(futureRefs.innerMap((f: Tree) => q"${Constants.variable}($f)"))
        case GraftValue =>
          error(pos, "Graft[_] argument types are not usable from within an Orclave.")
          coreFunc(futureRefs.innerMap((f: Tree) => q"???"))
      }

      lazy val forceCallAndBodies: Tree = bodyRefs.foldLeft(forceAndCall)((e, f) => {
        val r = q"$e.|||($f)"
        r.setPos(pos)
        r
      })

      q"""
      {
        ..$graftValDefs; 
        $forceCallAndBodies
      }
      """
    }

    val excludedClasses = {
      import TypeTrees._
      Set(OrcLowPriorityImplicitsSym, OrcSym, FutureSym)
    }

    def shouldBeLifted(sym: Symbol): Boolean = {
      //val s = f.symbol
      if (sym.isPackage || sym.isModule || sym.isClass) {
        false
      } else if (sym.isMethod) {
        val s = sym.asMethod
        val cls = s.owner
        !(excludedClasses contains cls)
      } else
        true
    }
    def shouldBeLifted(t: Tree): Boolean = {
      assume(t.symbol != null && t.symbol != NoSymbol)
      shouldBeLifted(t.symbol)
    }
    
    var currentApi: TypingTransformApi = _
    
    def recur(tree: Tree) = {
      apply(tree, currentApi)
    }

    def typecheck(tree: Tree) = {
      currentApi.typecheck(tree)
    }
    
    val typeFixerSymbols =  {
      import Constants._
      Set(
        futureInOrcContextSym,
        scalaInOrcContextSym,
        futureInScalaContextSym,
        orcInScalaContextSym
        )
    }

    def apply(tree: Tree, api: TypingTransformApi): Tree = {
      import Constants._, TypeTrees._
      assert(currentApi == null || currentApi == api)
      currentApi = api
      import api.{default, atOwner, currentOwner}

      val currentPos = tree.pos
      val result = tree match {
        // Remove type fixers
        case q"${ f @ q"$prefix.$n" }[$_]($e)" if typeFixerSymbols contains f.symbol =>
          //println(s"orcInScalaContext $orcInScalaContext $e ${showRaw(tree)}")
          recur(e)
        
        // Handle literals and variables
        case Literal(Constant(_)) | Ident(_) | This(_) =>
          def rebuilt: Tree = tree match {
            case Literal(Constant(v)) =>
              Literal(Constant(v))
            case Ident(n) =>
              Ident(n)
            case This(n) =>
              This(n)
          }
          try {
            buildKindConvertion(ValueKind(tree.tpe), OrcValue)(rebuilt)
          } catch {
            case _: IllegalArgumentException =>
              throw new IllegalArgumentException(s"OrcTransform cannot transform Graft[_] values\n${showRaw(tree)}")
          }
        
        // Special congruence rule for branch (map)
        case q"${callee @ q"$prefix.$f"}[..$targs](${lambda @ q"($x) => $e"})" if callee.symbol == Orc_mapSym =>
          //println(s"Call ${callee.symbol} $lambda")
          val e1 = atOwner(lambda, currentOwner) { recur(e) }
          // This untypecheck should always be safe because none of the problem cases are in argument positions.
          q"${recur(prefix)}.$f[..$targs]((${untypecheck(x)}) => ${e1})"
        
        // Rules for handling calls.
        case q"${ f @ Ident(n) }[..$targs](...$argss)" =>
          //println(s"Call ${f.symbol} $n $argss")
          // TODO: The types provided here are not instantiated for based on the targs. That should be done before passing them here.
          if (shouldBeLifted(f))
            buildApply(argss, f.tpe.paramLists.innerMap(_.info), f.tpe.finalResultType, currentPos)(argss => q"${Ident(n)}[..$targs](...$argss)")
          else
            untypecheck(default(tree))
        case q"${ f @ q"$prefix.$n" }[..$targs](...$argss)" =>
          //println(s"Call ${f.symbol} $prefix $n $argss")
          if (shouldBeLifted(f))
            buildApply(List(prefix) :: argss, List(prefix.tpe) :: f.tpe.paramLists.innerMap(_.info), f.tpe.finalResultType, currentPos) {
              argss =>
                val List(p) :: realArgss = argss
                q"$p.$n[..$targs](...$realArgss)"
            }
          else
            untypecheck(default(tree))
        
        case t @ TypeTree() =>
          t
        case t =>
          error(currentPos, s"Unsupported expression or statement in Orclave: ${showRaw(t)}")
          t
      }
      result.setPos(currentPos)
      //println(s"$tree ===> $result")
      result
    }
  }

  def orcExpr(o: Tree)(evidence: Tree): Tree = {
    //println(s"Start:\n$o\n========")
    val r = typingTransform(o)(OrcTransformer())
    //println(s"Final:\n$r\n========")
    r
  }
}
