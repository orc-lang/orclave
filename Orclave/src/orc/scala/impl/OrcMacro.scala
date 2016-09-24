package orc.scala.impl

import scala.reflect.macros.blackbox.Context
import orc.scala.Orc
import scala.concurrent.Future
import scala.annotation.compileTimeOnly
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
 * - Insert types manual if you have them (setType).
 * - Always set position if possible.
 * - Always type check as you go using typedTransform.
 * - Process elements in order (since typing has side effects.
 * - Remember that trees have types that can be different from the symbol to which they refer.
 */

class OrcMacro(val c: Context) extends OwnerSplicer {
  /* Transformations:
   * 
   * Introduce futures:
   * 
   * val x1 = e1
   * ...
   * val xn = en
   * f
   * 
   *   ====>
   * 
   * val x1 = e1
   * ...
   * val xn = en
   * x1.body ||| ... ||| xn.body |||
   * [xi |-> xi.future]f
   * 
   * Lift Futures: (some args may be non-future)
   * 
   * f(e1: Future[T1], ..., en: Future[Tn])
   * 
   *   ====>
   * 
   * variable(FutureUtil.tuple(e1, ..., en)).map(t => {
   *   val (y1, ..., yn) = t
   *   f(y1, ..., yn) 
   * })
   * 
   * Lift Orcs: (some args may of other kinds handled in their appropriate way)
   * 
   * f(e1: Orc[T1], ..., en: Orc[Tn])
   * 
   *   ====>
   * 
   * val x1 = e1.graft
   * x1.body ||| ... ||| xn.body |||
   * variable(FutureUtil.tuple(x1.future, ..., xn.future)).map(t => {
   *   val (y1, ..., yn) = t
   *   f(y1, ..., yn) 
   * })
   * 
   */

  // TODO: future forcing, especially at non-Orc calls.
  // TODO: Handle references to variables from outside the orclave correctly. This should include handling futures. 

  import c._
  import c.universe._
  //import compat._
  import c.internal._
  import c.internal.decorators._

  val qOrcMacroInternal = q"_root_.orc.scala.impl.OrcMacroInternal"
  val qorcToBeLifted = q"_root_.orc.scala.Orc.orcToBeLifted"
  val qvariable = q"_root_.orc.scala.Orc.variable"
  val tqScalaExpr = tq"_root_.orc.scala.impl.ScalaExpr"

  val qFutureUtil = reify(orc.scala.impl.FutureUtil).tree
  val tqOrc = tq"_root_.orc.scala.Orc"

  val OrcTypeConstructor = c.typeOf[Orc[Int]].typeConstructor
  val FutureTypeConstructor = c.typeOf[Future[Int]].typeConstructor
  val GraftTypeConstructor = c.typeOf[orc.scala.Graft[Int]].typeConstructor

  def println(s: String, pos: Position = enclosingPosition): Unit = c.echo(pos, s)

  sealed trait ScopeKind
  case object ScalaScope extends ScopeKind
  case object OrcScope extends ScopeKind
  case object FutureScope extends ScopeKind

  /** Build a new ValDef to replace `s` which applies graft to the body.
    *
    * `recur` is the is called on the new body expression.
    *
    * Returns the ValDef tree, the scope that usages are expected to have, and
    * the type of publications/bindings of the graft.
    */
  def buildGraftValDef(s: ValDef, recur: Tree => Tree = (x => x)): (Tree, ScopeKind, Type) = {
    val ValDef(mods, name, tpt, rhs) = s
    // Check if type is an Orc already
    val isOrcGraft = rhs.tpe.typeConstructor == OrcTypeConstructor
    // Build new ValDef based on old
    val nestedExpr = if (tpt.isEmpty) rhs else q"$rhs: $tpt"
    val newS = ValDef(mods, name, TypeTree(), q"$nestedExpr.graft")
    val newTpe = if (isOrcGraft) rhs.tpe.typeArgs.head else rhs.tpe
    newS.setPos(s.pos)
    newS.setSymbol(s.symbol)
    if (newS.symbol != NoSymbol && newS.symbol != null)
      newS.symbol.setInfo(appliedType(GraftTypeConstructor, newTpe))
    // TODO: Does this need to occur in a modified scope?
    val newS2 = recur(newS)
    val kind = (if (isOrcGraft) OrcScope else ScalaScope)
    (newS2, kind, newTpe)
  }

  /** Build a reference to the body of the graft represented as it's ValDef.
    */
  def buildGraftBodyRef(s: Tree): Tree = {
    val ValDef(_, n, _, _) = s
    val x = s.symbol
    val xi = if (s.symbol != NoSymbol) Ident(x) else Ident(n)
    // TODO: Figure out why setType is needed. 
    // Without the inferred type is Int implying that somewhere the symbol or name in x is associated with it's old type.
    // The same problem appears in the Ident case below.
    if (s.symbol != NoSymbol)
      xi.setType(x.info)
    val r = q"${xi}.body"
    r.setPos(s.pos)
    r
  }

  /** Build a reference to the graft future of `s` (the graft ValDef).
    *
    * @param k  Specify if the returned expression should be for Scala (type T) or Orc (type Orc[T]).
    * @param _i An Ident to reuse and get position from. If it is not specified a new Ident is created.
    */
  def buildGraftFutureRef(s: Either[Symbol, TermName], k: ScopeKind, _tpe: Type = null, _i: Ident = null) = {
    val i = if (_i != null) _i else
      s match {
        case Left(s)  => Ident(s)
        case Right(n) => Ident(n)
      }
    val tpe = if (_tpe != null) _tpe else
      s match {
        case Left(s)  => s.info
        case Right(n) => NoType
      }

    assume(s match {
      case Left(s)  => s == i.symbol
      case Right(n) => n == i.name
    })

    // TODO: Figure out why setType is needed. 
    if (tpe != NoType)
      i.setType(tpe)
    val r = k match {
      case ScalaScope =>
        q"$qOrcMacroInternal.futureToBeLifted(${i}.future)"
      case OrcScope =>
        q"$qvariable(${i}.future)"
      case FutureScope =>
        q"${i}.future"
    }
    r.setPos(if (_i != null) i.pos else enclosingPosition)
    r
  }

  def orcExprJunk(o: Tree): Tree = {
    val e = q"""
    {
      val t = 10;
      t
    }"""
    // Adding "typecheck" around e causes a failure. Why?
    val r = typecheck(e)
    println(s"r = $r")
    r
    //orcExpr1(o)
  }

  /** Phase 1: Transform all defs to their Orc form.
    */
  def orcExpr(o: Tree): Tree = {
    var graftVariables = Map[TermName, ScopeKind]()
    def transformFunc(tree: Tree, api: TypingTransformApi) = {
      import api._
      tree match {
        case Block(stats, expr) => {
          // Check for non-definition expressions in the block.
          for (s <- stats if !s.isDef) {
            error(s.pos, "[Orclave] Expression results cannot be ignored in Orc.")
          }

          // Check for non-val definitions since we don't support them yet.
          for (s <- stats) {
            s match {
              case ValDef(_, _, _, _) => ()
              case s if s.isDef =>
                error(s.pos, "[Orclave] Currently only val declarations are supported in Orc.")
              case _ => ()
            }
          }

          if (true) {
            // If there are no errors yet do the actual transform.

            val origTpe = tree.tpe

            // Replace vals
            val newStats = for (s @ ValDef(mods, name, tpt, rhs) <- stats) yield {
              val (t, k, _) = buildGraftValDef(s, recur)
              // Mark this definition as being a graft variable
              graftVariables += (name -> k)
              t
            }
            // Recur on expression then build the new expression
            // TODO: Does this need to occur in a modified scope?
            val expr2 = recur(expr)
            val newExpr = newStats.foldLeft(expr2)((e, vl) => {
              val r = q"$e.|||(${buildGraftBodyRef(vl)})"
              r.setPos(e.pos)
              r
            })
            // Rebuild the block
            val r = Block(newStats, newExpr)
            //println(s"> Running typecheck on: $r")
            // TODO: Does this need to occur in a modified scope?
            val rr = typecheck(r)
            //println(s"< Done running typecheck on: $rr")
            rr
          } else {
            default(tree)
          }
        }
        case i @ Ident(n) if n.isTermName && (graftVariables contains n.toTermName) =>
          // For any variable that has been graft converted update the type and wrap for correct typing. 
          typecheck(buildGraftFutureRef(Left(i.symbol), graftVariables(n.toTermName), _i = i))
        case t => default(t)
      }
    }

    //println(s"Phase 1 IN:\n$o")
    val res = typingTransform(o)(transformFunc)
    //println(s"Phase 1 OUT:\n$res")
    res
  }

  case class ReplacementTree(t: Tree)

  /** Phase 2: Lift references to Orc and Future in scala expressions using forces.
    */
  def scalaExpr(v: Tree): Tree = {
    // TODO: Could generate an anonymous subclass to avoid megamorphic call sites at a code size cost
    //println(s"Phase 2 IN:\n$v")

    // 1: Perform ANF transformation on v
    // 2: Perform graft conversion on the created vals (using code shared with orclave macro)
    // 3: Build force operations for each graft body

    // TODO: For now we are only doing step 3. The other steps should probably be separate.

    // TODO: For each reference to an something that needs forcing collect information and generate a force operation using FutureUtil.tuple.
    //       For Orc reference we will need to build a graft of sorts, but it may be able to be simplified. 

    var leadingStatements = Vector[Tree]()
    var parallelExpressions = Vector[Tree]()
    var forcePairs = Vector[(Tree, (TermSymbol, Type))]()
    val expressionCollector = new Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case Apply(TypeApply(callee @ Select(_, n), _), List(e)) if n == TermName("orcToBeLifted") =>
          //println(s"orc: $e", tree.pos)
          if (currentOwner != enclosingOwner)
            error(e.pos, s"Orclave cannot handle orc expressions in this position (should be under $enclosingOwner, was under $currentOwner)")

          // Build graft operation for Orc expression
          val n = freshName(TermName("_tmpgraft"))
          val vl = ValDef(NoMods, n, TypeTree(), q"$e")
          val sym = newTermSymbol(enclosingOwner, n)
          vl.setSymbol(sym)
          val (vl1, OrcScope, tpe) = buildGraftValDef(vl)
          leadingStatements :+= vl1

          // Place graft body so that it can be put in parallel with the expression.
          parallelExpressions :+= buildGraftBodyRef(vl1)

          // Build forcing operation for the new graft.
          val fn = freshName(TermName("_forcedval"))
          val fsym = newTermSymbol(enclosingOwner, fn)
          fsym.setInfo(tpe)
          val toForce = buildGraftFutureRef(Left(sym), FutureScope, _tpe = tpe)
          forcePairs :+= (toForce -> (fsym, tpe))

          // Mark this tree as replaced by a specific symbol reference.
          tree.updateAttachment(ReplacementTree(q"$fsym"))

        case Apply(TypeApply(callee @ Select(_, n), _), List(e)) if n == TermName("futureToBeLifted") =>
          //println(s"future: $e", tree.pos)
          // Build forcing operation for future.
          val fn = freshName(TermName("_forcedval"))
          val fsym = newTermSymbol(enclosingOwner, fn)
          fsym.setInfo(e.tpe.typeArgs.head)
          val toForce = q"$e"
          forcePairs :+= (toForce -> (fsym, tree.tpe))

          // Mark this tree as replaced by a specific symbol reference.
          tree.updateAttachment(ReplacementTree(q"$fsym"))

        case t =>
          super.traverse(t)
      }
    }
    expressionCollector.atOwner(enclosingOwner) {
      expressionCollector.traverse(v)
    }

    /*println(s"""Traversal results:
      |leadingStatements = $leadingStatements
      |parallelExpressions = $parallelExpressions
      |forcePairs = $forcePairs
      """.stripMargin)
		*/
    lazy val replacedV = typingTransform(v) { (tree, api) =>
      import api._
      tree.attachments.get[ReplacementTree] match {
        case Some(ReplacementTree(t)) =>
          typecheck(t.setPos(tree.pos))
        case None =>
          default(tree)
      }
    }

    val (toFutures, forceResultVars) = forcePairs.unzip

    lazy val core = q"new $tqScalaExpr(() => ${ownerSplice(replacedV)})"

    lazy val orcInner: Tree = toFutures match {
      case Seq() =>
        core
      case Seq(f) =>
        val (rv, rt) = forceResultVars.head
        lazy val rvParam = ValDef(Modifiers(Flag.PARAM), rv.name, TypeTree(), EmptyTree)
        q"""
        $qvariable($f).map(($rvParam) => { 
          ${ownerSplice(core)}
        })"""
      case toFutures =>
        lazy val t = freshName(TermName("_tmptuple"))
        lazy val tParam = ValDef(Modifiers(Flag.PARAM), t, TypeTree(), EmptyTree)
        lazy val forceBinders: Vector[Tree] = forceResultVars.zipWithIndex.map(p => {
          val ((rv, rt), i) = p
          ValDef(Modifiers(), rv.name, TypeTree(rt), q"$t.${TermName(s"_${i + 1}")}")
          //val r = Bind(x.name, pq"_ : ${TypeTree(x.info)}")
          //r.setSymbol(x)
          //r
        })
        q"""
        $qvariable($qFutureUtil.tuple(..$toFutures)).map(($tParam) => { 
          ..$forceBinders;
          ${ownerSplice(core)}
        })"""
    }

    lazy val orcInner1: Tree = q"$orcInner : $tqOrc[${v.tpe}]"

    lazy val orcInner2: Tree = parallelExpressions.foldLeft(orcInner1)((e, f) => {
      val r = q"$e.|||($f)"
      r.setPos(e.pos)
      r
    })

    val newExpr = Block(leadingStatements.toList, orcInner2)

    println(s"Phase 2 OUT:\n$newExpr")

    newExpr
  }
}
