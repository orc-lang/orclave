package orc.scala.impl

import scala.reflect.macros.blackbox.Context
import orc.scala.Orc
import scala.concurrent.Future
import scala.annotation.compileTimeOnly

object OrcMacroInternal {
  import scala.language.experimental.macros

  def orcExprPhase2[T](o: Orc[T]): Orc[T] = macro OrcMacro.orcExprPhase2

  @compileTimeOnly("[Orclave ICE] Future value can only be used as Future[T] outside an orclave. An expression was used " +
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

class OrcMacro(val c: Context) {
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

  // TODO: Remove use of untypecheck. This may require diging into internal so as to avoid symtable issues.
  //       However it may well be possible to generate type and symbol information from the type information we have without running the type checker.

  // TODO: future forcing, especially at non-Orc calls.

  import c._
  import c.universe._
  //import compat._
  import c.internal._
  import c.internal.decorators._

  val qOrcMacroInternal = q"_root_.orc.scala.impl.OrcMacroInternal"
  val qGraft = tq"_root_.orc.scala.Graft"
  val qorcToBeLifted = q"orc.scala.Orc.orcToBeLifted"

  val OrcTypeConstructor = c.typeOf[Orc[Int]].typeConstructor
  val FutureTypeConstructor = c.typeOf[Future[Int]].typeConstructor
  val GraftTypeConstructor = c.typeOf[orc.scala.Graft[Int]].typeConstructor

  def println(s: String, pos: Position = enclosingPosition): Unit = c.echo(pos, s)

  def orcExpr(o: Tree): Tree = {
    if (true) {
      orcExprPhase1(o)
    } else {
      typingTransform(o)((tree, api) => {
        import api._
        println(s"$currentOwner >> $tree")
        tree match {
          case i @ Ident(n) if s"${i.tpe}" contains "Int" =>
            typecheck(q"$i.hashCode()")
          case _ =>
            default(tree)
        }
      })
    }
  }

  /** Phase 1: Transform all defs to their Orc form.
    */
  def orcExprPhase1(o: Tree): Tree = {

    val tpe = o.tpe

    var graftVariables = Set[TermName]()
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
            val (newStats, vars) = (for (s @ ValDef(mods, name, tpt, rhs) <- stats) yield {
              // Check if type is an Orc already
              val isOrcGraft = rhs.tpe.typeConstructor == OrcTypeConstructor
              // Mark this definition as being a graft variable
              graftVariables += name
              // Build new ValDef based on old
              val nestedExpr = if (tpt.isEmpty) rhs else q"$rhs: $tpt"
              val newS = ValDef(mods, name, TypeTree(), q"$nestedExpr.graft")
              val newTpe = if (isOrcGraft) rhs.tpe.typeArgs.head else rhs.tpe
              newS.setPos(s.pos)
              newS.setSymbol(s.symbol)
              newS.symbol.setInfo(appliedType(GraftTypeConstructor, newTpe))
              // TODO: Does this need to occur in a modified scope?
              val newS2 = recur(newS)
              (newS2, newS2.symbol)
            }).unzip
            // Recur on expression then build the new expression
            // TODO: Does this need to occur in a modified scope?
            val expr2 = recur(expr)
            val newExpr = vars.foldLeft(expr2)((e, x) => {
              val xi = Ident(x)
              // TODO: Figure out why setType is needed. 
              // Without the inferred type is Int implying that somewhere the symbol or name in x is associated with it's old type.
              // The same problem appears in the Ident case below.
              xi.setType(x.info)
              val r = q"$e.|||(${xi}.body)"
              r.setPos(e.pos)
              r
            })
            // Rebuild the block
            val r = Block(newStats, newExpr)
            // TODO: Does this need to occur in a modified scope?
            typecheck(r)
          } else {
            default(tree)
          }
        }
        case Apply(TypeApply(callee @ Select(_, n), _), args) if n == TermName("orcToBeLifted") =>
          // TODO: Remove string check hack. This should check full path.
          //   println(s"Found orcToBeLifted as $callee (${showRaw(callee)})")
          // Don't touch orcToBeLifted subexpressions. So we don't recur here.
          tree
        case i @ Ident(n) if n.isTermName && (graftVariables contains n.toTermName) =>
          // For any variable that has been graft converted update the type and wrap for correct typing. 
          //val resTpe = i.symbol.info.typeArgs.head
          // TODO: Figure out why setType is needed. 
          i.setType(i.symbol.info)
          val r = typecheck(q"$qOrcMacroInternal.futureToBeLifted(${i}.future)")
          r.setPos(i.pos)
          r
        case t => default(t)
      }
    }

    println(s"Phase 1:\n$o")
    val newO = typingTransform(o)(transformFunc)
    val res = q"$qOrcMacroInternal.orcExprPhase2(${Block(Nil, newO)})"
    res
  }

  def orcExprPhase2(o: Tree): Tree = {
    println(s"Phase 2:\n$o")
    o
  }
}

