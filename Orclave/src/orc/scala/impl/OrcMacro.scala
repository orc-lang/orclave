package orc.scala.impl

import scala.reflect.macros.blackbox.Context
import orc.scala.Orc
import scala.concurrent.Future
import scala.annotation.compileTimeOnly
import orc.util.ListExtensions._
import scala.collection.mutable

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
  import c.universe.{typeOf => _, _}
  //import compat._
  import c.internal._
  import c.internal.decorators._

  object TypeTrees {
    val ScalaExpr = typecheck(tq"_root_.orc.scala.impl.ScalaExpr", TYPEmode)
    val Orc = typecheck(tq"_root_.orc.scala.Orc", TYPEmode)
    val OrcSym = Orc.symbol
    //val OrcObj = typecheck(tq"_root_.orc.scala.Orc.type", TYPEmode)
    val OrcObjSym = OrcSym.companion.info.typeSymbol
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
    val scalaclave = q"_root_.orc.scala.Orc.scalaclave"
    val scalaclaveSym = OrcObjSym.info.member(TermName("scalaclave"))
    val FutureUtil = reify(orc.scala.impl.FutureUtil).tree
    val orcToBlockingIterable = q"_root_.orc.scala.Orc.orcToBlockingIterable"
  }


  object Types {
    val Orc = c.typeOf[orc.scala.Orc[Unit]].typeConstructor
    val Future = c.typeOf[scala.concurrent.Future[Unit]].typeConstructor
    val Graft = c.typeOf[orc.scala.Graft[Unit]].typeConstructor
  }

  var currentIndent = 0
  var currentPos = enclosingPosition
  def withIndent[T](f: => T): T = {
    currentIndent += 2
    try {
      f
    } finally {
      currentIndent -= 2
    }
  }
  def withIndent[T](t: Position)(f: => T): T = {
    currentIndent += 2
    val oldPos = currentPos
    currentPos = t
    try {
      f
    } finally {
      currentIndent -= 2
      currentPos = oldPos
    }
  }
  def println(s: String, pos: Position = currentPos): Unit = c.echo(pos, " "*(currentIndent*2) + s)

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

  implicit class SymbolAdds(val s: c.Symbol) {
    def kind: ValueKind = {
      val attachedKind = s.attachments.get[ValueKind]
      val typeKind = Option(s.info).map(_.kind)
      // If we have the symbol kind use that, otherwise use attached/type kind if they agree otherwise use whatever we have.
      (attachedKind, typeKind) match {
        /*case (Some(k1), Some(k2)) if k1 == k2 =>
          k1
        case (Some(k1), Some(k2)) =>
          throw new IllegalStateException(s"Kinds ($k1, $k2) disagree on $s.")
          */
        case (Some(k1), _) =>
          // Allow disagreement and give attached priority.
          k1
        case (_, Some(k1)) =>
          k1
        case (None, None) =>
          throw new IllegalStateException(s"Kind unknown (no type or kind set): $s")
      }
    }
    
    def setKind(k: ValueKind): s.type = {
      s.updateAttachment(k)
      s
    }
  }
  
  implicit class TreeAdds(val t: c.Tree) {
    def setGenerated(): t.type = {
      t.updateAttachment(OrclaveGenerated)
      t
    }
    def isGenerated: Boolean = {
      t.attachments.get[OrclaveGenerated].isDefined
    }

    def kind: ValueKind = {
      val attachedKind = t.attachments.get[ValueKind]
      val typeKind = Option(t.tpe).map(_.kind)
      val symbolKind = t match {
        // Only accept override from attached kind on symbols.
        case _ if t.symbol != null && t.symbol != NoSymbol =>
          t.symbol.attachments.get[ValueKind]
        case _ =>
          None
      }
      val symbolTypeKind = t match {
        // Only accept override from attached kind on symbols.
        case _ if t.symbol != null && t.symbol != NoSymbol =>
          Some(t.symbol.kind)
        case _ =>
          None
      }
      // If we have the symbol kind use that, otherwise use attached/type kind if they agree otherwise use whatever we have.
      (attachedKind, typeKind, symbolKind, symbolTypeKind) match {
        case (k1, k2, Some(k3), _) =>
          if(k3 != FutureValue && k2 != Some(k3) && k1 != Some(k3))
            info(t.pos, s"ICE: Kind should only be overriden to future (used by graft). ($k1, $k2, $k3)", true)
          k3
        case (Some(k1), Some(k2), _, _) if k1 == k2 =>
          k1
        case (Some(k1), Some(k2), _, _) =>
          throw new IllegalStateException(s"Kinds ($k1, $k2) disagree on $t.")
        case (Some(k1), _, _, _) =>
          k1
        case (_, Some(k1), _, _) =>
          k1
        // If it's not set anywhere else then get the kind for the type of the symbol
        case (_, _, _, Some(k1)) =>
          k1
        case (None, None, None, None) =>
          throw new IllegalStateException(s"Kind unknown (no type or kind set): $t (tpe=${t.tpe}, symbol=${t.symbol})")
      }
    }
    
    def kindOpt = {
      try {
        Some(kind)
      } catch {
        case _: IllegalStateException =>
          None
      }
    }
  

    def setKind(k: ValueKind): t.type = {
      t.updateAttachment(k)
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
  
  class FindFreeVariables(predicate: Ident => Boolean) extends Traverser {
    private val resultBuilder = Set.newBuilder[Symbol]
    private val localSymbols = mutable.Set[Symbol]()
    
    def isVariableSymbol(n: Name, s: Symbol) = {
      s.isTerm &&
      !s.isMethod &&
      !s.isModule &&
      !s.isClass &&
      !s.isPackage &&
      n != termNames.WILDCARD
    }
    
    override def traverse(tree: Tree) = tree match {
      case i @ Ident(n) if isVariableSymbol(n, i.symbol) && !localSymbols.contains(i.symbol) && predicate(i) =>
        resultBuilder += i.symbol
      case ValDef(_, _, _, body) =>
        assert(tree.symbol != null && tree.symbol != NoSymbol)
        localSymbols += tree.symbol
        atOwner(tree.symbol) { traverse(body) }
      case Bind(_, body) =>
        assert(tree.symbol != null && tree.symbol != NoSymbol)
        localSymbols += tree.symbol
        atOwner(tree.symbol) { traverse(body) }
      case _ =>
        super.traverse(tree)
    }
    
    def result = resultBuilder.result()
  }
  
  def findFreeVariables(tree: Tree, predicate: Ident => Boolean) = {
    val trav = new FindFreeVariables(predicate)
    trav.traverse(tree)
    trav.result
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
     * + Graft
     * + Congruence on calls to Orc
     * + scalaclaves
     * + Assignment statements as unit returning sitecalls
     * - def
     * - Otherwise-like combinator for exception handling 
     */
    // TODO: Finish todos above
    
    object Construct {
      def parallel(ts: Iterable[Tree]) = {
        ts.reduceLeft((e, a) => {
          val r = q"$e.|||($a)"
          r.setPos(a.pos)
          r
        })
      }
    }
    
    // TODO: This uses untypecheck. 
    // While lazy will be disallowed anyway and case classes don't seem useful, match destructors seem important eventually. 

    def buildKindConvertion(sk: ValueKind, tk: ValueKind)(t: Tree): Tree = {
      import Constants._
      val r = (sk, tk) match {
        case (k1, k2) if k1 == k2 => t
        case (ScalaValue, OrcValue) =>
          q"$scalaExpr($t)"
        case (FutureValue, OrcValue) =>
          q"$variable($t)"
        case _ =>
          throw new IllegalArgumentException(s"Illegal conversion requested: from $sk to $tk")
      }
      r.setKind(tk)
      r
    }

    /** Return the set of kinds that this expression is convertable to.
     */
    def convertableTo(t: Tree): Set[ValueKind] = {
      if (t.symbol != null && t.symbol.isPackage) {
        Set(ScalaValue)
      } else {
        Set(OrcValue, FutureValue, ScalaValue)
      }
    }
    
    
    abstract class GraftPatternBase(graftBody: Tree, tpe: Type) {
      assume(graftBody.kind == OrcValue)
      
      val graftName: TermName
      
      lazy val valDef = {
        // Usually correct type: tpe.widen.withKind(GraftValue)
        // However this type seems to sometimes contain type variables
        // TODO: Fix this to generate type annotation. Need to fix issue in apply cases first.
        val r = ValDef(Modifiers(Flag.SYNTHETIC), graftName, TypeTree(), q"$graftBody.graft")
        r.setGenerated()
        r.setPos(graftBody.pos)
        r
      }
      def futureExpr = q"$graftName.future"
      def body = q"$graftName.body"
      
      override def toString(): String = s"GraftPattern(..., $graftName, $tpe)"
    }
    
    case class GraftPattern(graftBody: Tree, graftName: TermName, tpe: Type) extends GraftPatternBase(graftBody, tpe)

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
    def buildApply(argss: List[List[Tree]], argTypess: List[List[Type]], returnType: Type, expectedKind: ValueKind, pos: Position)(coreFunc: List[List[Tree]] => Tree): Tree = {
      //val formalKindss = argTypess.innerMap(_.kind)
      //val returnKind = returnType.kind
      
      for(argTypes <- argTypess; argType <- argTypes) argType.kind match {
        case OrcValue =>
          error(pos, "Orc[_] argument types are not allowed on function called from within an Orclave.")
        case GraftValue =>
          error(pos, "Graft[_] argument types are not allowed on function called from within an Orclave.")
        case _ => ()
      }

      def refName(a: Tree) = {
        if (a.symbol != null && a.symbol != NoSymbol)
          a.symbol.name.encodedName.toString()
        else
          "expr"
      }
      
      
      sealed trait Argument {
        val name: String
        def freshRefName(r: String) = freshName(TermName(s"${name}_$r"))
        def argumentExpr: Tree
      }
      trait ForcedArg extends Argument {
        def futureExpr: Tree
        
        val forcedName: TermName = freshRefName("forced")
        def argumentExpr = q"$forcedName" 
      }
      case class GraftArg(graftBody: Tree, name: String, tpe: Type) extends GraftPatternBase(graftBody, tpe) with Argument with ForcedArg {
        val graftName = freshRefName("graft")
        override def toString(): String = s"GraftArg(..., $name, $tpe)"
      }
      case class ScalaArg(scalaExpr: Tree, name: String) extends Argument {
        assume(scalaExpr.kind == ScalaValue)
        def argumentExpr = scalaExpr
      }
      case class FutureArg(futureExpr: Tree, name: String) extends Argument with ForcedArg {
        assume(futureExpr.kind == FutureValue)
      }
      
      val arguments = argss.innerMap { arg =>
        val n = refName(arg)
        val e = recur(arg, Set(OrcValue, FutureValue, ScalaValue))
        e.kind match {
          case OrcValue =>
            GraftArg(e, n, null)
          case ScalaValue =>
            ScalaArg(e, n)
          case FutureValue =>
            FutureArg(e, n)
          case GraftValue =>
            error(arg.pos, "Graft[_] argument are not allowed within an Orclave.")
            FutureArg(q"$e.future", n)
        }
      }
      
      //println(s"apply: ${arguments.mkString(",")}", pos)

      lazy val core = {
        val c = coreFunc(arguments.innerMap(_.argumentExpr))
        buildKindConvertion(returnType.kind, expectedKind)(c)
      }
      
      def force(args: Seq[ForcedArg], core: Tree) = {
        args match {
          case Seq() =>
            core
          case Seq(f) =>
            assert(expectedKind == OrcValue)
            val forcedName = f.forcedName
            val futureRef = f.futureExpr
            q"""
            ${Constants.variable}($futureRef).map((${ValDef(Modifiers(Flag.PARAM), forcedName, TypeTree(), EmptyTree)}) => { 
              $core
            })
            """
          case forceArgs =>
            assert(expectedKind == OrcValue)
            lazy val t = freshName(TermName("tuple"))
            lazy val tParam = ValDef(Modifiers(Flag.PARAM), t, TypeTree(), EmptyTree)
            lazy val forceBinders = forceArgs.zipWithIndex.map(p => {
              val (fa, i) = p
              val r = ValDef(Modifiers(Flag.SYNTHETIC), fa.forcedName, TypeTree(), q"$t.${TermName(s"_${i + 1}")}")
              r.setGenerated()
              r
            })
            q"""
            ${Constants.variable}(${Constants.FutureUtil}.tuple(..${forceArgs.map(_.futureExpr)})).map(($tParam) => { 
              ..$forceBinders;
              $core
            })
            """
        }
      }
      
      lazy val forceCore = force(arguments.flatten.collect { case f: ForcedArg => f }, core)
      
      lazy val graftArgs = arguments.flatten.collect { case g: GraftArg => g }
      
      assert(expectedKind == OrcValue)
      lazy val forceCoreBodies: Tree = Construct.parallel(forceCore +: graftArgs.map(_.body))
      
      val r = q"""
      {
        ..${graftArgs.map(_.valDef)}; 
        $forceCoreBodies
      }
      """
      r.setKind(expectedKind)
      r
    }

    val excludedClasses = {
      import TypeTrees._
      Set(OrcLowPriorityImplicitsSym, OrcSym, FutureSym, OrcObjSym)
    }

    def isOrcPrimitive(sym: Symbol): Boolean = {
      if (sym.isMethod) {
        val s = sym.asMethod
        val cls = s.owner
        (excludedClasses contains cls)
      } else
        false
    }
    def isOrcPrimitive(t: Tree): Boolean = {
      assume(t.symbol != null && t.symbol != NoSymbol)
      isOrcPrimitive(t.symbol)
    }
    
    val kindMap = {
      import TypeTrees._
      Map(OrcLowPriorityImplicitsSym -> ScalaValue, OrcSym -> OrcValue, FutureSym -> FutureValue, OrcObjSym -> ScalaValue)
    }
    
    def kindForOrcPrimitivePrefix(sym: Symbol): ValueKind = {
      assume(isOrcPrimitive(sym))
      val s = sym.asMethod
      val cls = s.owner
      kindMap(cls)
    }
    def kindForOrcPrimitivePrefix(t: Tree): ValueKind = {
      assume(t.symbol != null && t.symbol != NoSymbol)
      kindForOrcPrimitivePrefix(t.symbol)
    }
    
    var currentApi: TypingTransformApi = _
    
    /** Recur using `currentApi`.
     * 
     * @see apply
     */
    def recur(tree: Tree, allowedKinds: Set[ValueKind]) = {
      apply(tree, currentApi, allowedKinds)
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
    
    def stripTypeFixers(t: Tree): Tree = t match {
      case q"${ f @ q"$prefix.$n" }[$_]($e)" if typeFixerSymbols contains f.symbol =>
        stripTypeFixers(e)
      case _ =>
        t
    }

    /** Tranform `tree` to an Orc[T] type.
     */
    def apply(tree: Tree, api: TypingTransformApi): Tree = {
      apply(tree, api, Set(OrcValue))
    }
    
    /** Transform `tree` from it's current kind to one of the kinds listed in `allowedKinds`.
     * 
     * 
     */
    def apply(tree: Tree, api: TypingTransformApi, extAllowedKinds: Set[ValueKind]): Tree = {
      assume(!extAllowedKinds.isEmpty)
      import Constants._, TypeTrees._
      assert(currentApi == null || currentApi == api)
      currentApi = api
      import api.{default, atOwner, currentOwner}
      
      val allowedKinds = convertableTo(tree) intersect extAllowedKinds
      assert(!allowedKinds.isEmpty)

      val currentPos = tree.pos

      def handleApply(forig: Tree, f: Tree, targs: List[Tree], argss: List[List[Tree]], prefixArgs: List[List[Tree]], prefixArgTypes: List[List[Type]])(coreFunc: List[List[Tree]] => Tree) = {
        if (isOrcPrimitive(forig)) {
          q"$f[..$targs](...${argss.innerMap(recur(_, Set(OrcValue)))})".setKind(OrcValue)
        } else if (allowedKinds contains OrcValue) {
          buildApply(prefixArgs ::: argss, prefixArgTypes ::: forig.tpe.paramLists.innerMap(_.info), forig.tpe.finalResultType, OrcValue, currentPos)(coreFunc)
        } else if (allowedKinds contains tree.kind) {
          // Handle references that are not allowed to be Orc. Just hope all the subexpressions can be converted.
          q"$f[..$targs](...${argss.innerMap(a => recur(a, Set(a.kind)))})".setKind(tree.kind)
        } else {
          error(tree.pos, s"ICE: Cannot build proper kind. $tree ${tree.kind} $allowedKinds")
          q"$f[..$targs](...${argss.innerMap(a => recur(a, Set(a.kind)))})".setKind(tree.kind)
        }
      }

      val traceCalls = false
      if (traceCalls)
        println(s">> [${tree.kind} => {${allowedKinds.mkString(", ")}} (from ${extAllowedKinds.mkString(", ")}})] $tree", currentPos)
      
      val result = withIndent(currentPos) {
        val result = tree match {
          // Remove type fixers
          case q"${ f @ q"$prefix.$n" }[$_]($e)" if typeFixerSymbols contains f.symbol =>
            //println(s"Type fixer: ${f.symbol} $e ${tree}")
            recur(e, allowedKinds)
          
          // Handle literals and variables
          case Literal(Constant(_)) | Ident(_) | This(_) =>
            val (rebuilt: Tree, sym) = tree match {
              case Literal(Constant(v)) =>
                (tree, NoSymbol)
              case Ident(n) =>
                (Ident(n), tree.symbol)
              case This(n) =>
                //println(s"$n $tree ${tree.symbol}")
                (tree, tree.symbol)
            }
            
            val sourceKind = tree.kind
            val reducedAllowedKinds: Set[ValueKind] = if (sym.isPackage) {
              // Packages are not first class, so they must never be lifted.
              // TODO: Is there anything else that cannot be lifted?
              allowedKinds intersect Set(ScalaValue)
            } else {
              allowedKinds
            }
            val targetKind = {
              if(reducedAllowedKinds contains sourceKind)
                sourceKind
              else if(reducedAllowedKinds contains OrcValue)
                OrcValue
              else {
                error(tree.pos, s"ICE: Cannot find matching kinds. $tree $sourceKind $reducedAllowedKinds")
                sourceKind
              }
            }
            
            //println(s"$tree: ${tree.tpe}: $sourceKind => $targetKind ($sym)")
            try {
              buildKindConvertion(sourceKind, targetKind)(rebuilt)
            } catch {
              case _: IllegalArgumentException =>
                throw new IllegalArgumentException(s"OrcTransform cannot transform Graft[_] values\n${showRaw(tree)}")
            }
          
          // Scalaclave rule
          case q"${callee @ q"$prefix.$f"}[..$targs]($e)" if callee.symbol == Constants.scalaclaveSym =>
            //val e = stripTypeFixers(eRaw)
            // Find all free variables in e which are bound to futures
            val freeFutures = findFreeVariables(e, i => i.kind == FutureValue).toList
            //println(s"Free future $freeFutures")
            // Force all those variables before the scalaclave executes
            buildApply(List(freeFutures.map(Ident)), List(freeFutures.map(_.info.withoutKind)), e.tpe, OrcValue, e.pos) { argss =>
              val List(args) = argss
              val symBinders = for((futS, arg) <- (freeFutures zip args)) yield {
                val name = futS.name.toTermName
                val tpe = futS.info.withoutKind
                val sym = currentOwner.newTermSymbol(name, futS.pos, Flag.SYNTHETIC)
                sym.setInfo(tpe)
                val r = ValDef(Modifiers(Flag.SYNTHETIC), name, TypeTree(tpe), arg)
                r.setSymbol(sym)
                r
              }
              val symMap = (freeFutures zip symBinders.map(_.symbol)).toMap
              //println(s"Built: $symMap and:\n${symBinders.mkString("\n")}")
              // Remove calls to futureInScalaContext and orcInScalaContext around replaced variables from e
              // And replace all those variables in e with the result of forcing
              val liftSymbol = Set(orcInScalaContextSym, futureInScalaContextSym)
              object LiftingApply {
                def unapply(t: Tree): Option[(ValueKind, Tree)] = {
                  t match {
                    case q"$e: $tpe" =>
                      Some((tpe.kind, e))
                    case q"${ f @ q"$prefix.$n" }[$_]($e)" if liftSymbol contains f.symbol =>
                      Some((f.symbol.info.finalResultType.kind, e))
                    case _ => Some((t.kind, t))
                  }
                }
              }
              val e1 = typingTransform(e, currentOwner) { (reptree, repapi) =>
                val t = reptree match {
                  case LiftingApply(k, i @ Ident(name)) if symMap contains i.symbol =>
                    // TODO: This can generate runtime type errors.
                    // How do I make sure the type is correct?
                    //println(s"$i: $k ${i.kind}")
                    if (k != ScalaValue) {
                      // TODO: This should be a warning not an error. However, it's unclear how to correct the reference in this case.
                      error(i.pos, s"[Orclave] Reference to ${name.decodedName} must have expected type ${i.tpe.withoutKind}. Adding an explicit type annotation may help.")
                      i
                    } else {
                      repapi.typecheck(Ident(symMap(i.symbol)).setPos(i.pos))
                    }
                  case _ =>
                    repapi.default(reptree)
                }
                //println(s"Replacing $reptree with $t in scalaclave.")
                t
              }
              ownerSplice(Block(symBinders, e1), owner=currentOwner)
            }
            
          // Special congruence rule for primitives with lambda arguments
          case q"${callee @ q"$prefix.$f"}[..$targs](${lambda @ q"($x) => $e"})" if isOrcPrimitive(callee) =>
            //println(s"Call ${callee.symbol} $lambda")
            val e1 = atOwner(lambda, currentOwner) { recur(e, Set(OrcValue)) }
            // This untypecheck should always be safe because none of the problem cases are in argument positions.
            q"${recur(prefix, Set(kindForOrcPrimitivePrefix(callee)))}.$f[..$targs]((${untypecheck(x)}) => ${e1})".setKind(OrcValue)
          
          // Rules for handling calls.
          case q"${ f @ Ident(n) }[..$targs](...$argss)" =>
            //println(s"Call ${f.symbol} $n $argss (${shouldBeLifted(f)})")
            // TODO: The types provided here are not instantiated for based on the targs. That should be done before passing them here.
            handleApply(f, f, targs, argss, Nil, Nil) {
              argss => 
                q"${Ident(n)}[..$targs](...$argss)"
            }
          case q"${ f @ q"$prefix.$n" }[..$targs](...$argss)" =>
            val allowedPrefixKinds: Set[ValueKind] = {
              if (isOrcPrimitive(f))
                Set(kindForOrcPrimitivePrefix(f))
              else if (allowedKinds contains OrcValue)
                Set(OrcValue, FutureValue, ScalaValue)
              else
                Set(f.kind)
            }
            //println(s"Call ${f.symbol} $prefix $n $argss (${isOrcPrimitive(f)})  ${f.symbol.owner}")
            val f1 = q"${recur(prefix, allowedPrefixKinds)}.$n"
            handleApply(f, f1, targs, argss, List(List(prefix)), List(List(prefix.tpe))) {
              argss =>
                val List(p) :: realArgss = argss
                q"$p.$n[..$targs](...$realArgss)"
            }
          case q"$x = $v" =>
            // Strip the symbol from any x that is an ident.
            val newX = x match {
              case Ident(x) => Ident(x)
              case o =>
                error(o.pos, s"Assignment to $o not supported in Orclave.")
                o
            }
            buildApply(List(List(v)), List(List(v.tpe)), typeOf[Unit], OrcValue, currentPos) {
              argss =>
                val List(List(v1)) = argss
                q"$newX = $v"
            }

          // Rules for handling val in blocks
          case Block(stats, expr) =>
            assert(allowedKinds contains OrcValue)
            
            for (s <- stats) {
              s match {
                case q"val $x: $t = $e" =>
                  ()
                case q"var $x: $t = $e" =>
                  ()
                case _ =>
                  // TODO: Add advice for running statement after.
                  error(s.pos, s"Statements other than vals and vars are not supported in Orclaves. Use val _ = [stat] to run your statement in parallel or [stat] andthen ... to run in sequence.")
              }
            }
            
            if (!hasErrors) {
              // Build graft pattern for each
              val grafts = mutable.Buffer[GraftPattern]()
              // Build a pair of vals for each
              val newStats = stats.flatMap(s => s match {
                case q"val $x: $t = $e" =>
                  val pat = GraftPattern(recur(e, Set(OrcValue)), freshName(TermName(x + "_graft")), e.tpe)
                  grafts += pat
                  s.symbol.setKind(FutureValue)
                  Seq(pat.valDef, ValDef(Modifiers(Flag.SYNTHETIC), x, TypeTree(), pat.futureExpr).setGenerated())
                case q"var $x: $t = $e" =>
                  Seq(q"var $x: $t = $e")
              })
              // Recur
              val newExpr = Construct.parallel(recur(expr, Set(OrcValue)) +: grafts.map(_.body))
              q"{ ..$newStats; $newExpr }".setKind(OrcValue)
            } else {
              Block(stats.map(recur(_, Set(OrcValue))), recur(expr, Set(OrcValue)))
            }
              
            
          // Things I don't care about and can pass through.
          case t @ TypeTree() =>
            t
          case t =>
            error(currentPos, s"Unsupported expression in Orclave: ${showRaw(t)}")
            t
        }
        try {
          if(!hasErrors && !(allowedKinds contains result.kind)) {
            println(s"Kind not in allowed set: ${result.kind} not in $allowedKinds")
          }
        } catch {
          case _: IllegalStateException =>
            println(s"Result does not have a kind: $result")
        }
        result
      }
      result.setPos(currentPos)
      if (traceCalls)
        println(s"<< [${tree.kind} => ${result.kindOpt.getOrElse("?")}] ===> $result", currentPos)
      result
    }
  }

  def orcExpr(o: Tree)(evidence: Tree): Tree = {
    //println(s"Start:\n$o\n========")
    val r = typingTransform(o)(OrcTransformer())
    //println(s"Final:\n$r\n========")
    r
  }
  def orclave(o: Tree)(ctx: Tree, evidence: Tree): Tree = {
    val r = typingTransform(o)(OrcTransformer())
    q"${Constants.orcToBlockingIterable}($r)($ctx)"
  }
}
