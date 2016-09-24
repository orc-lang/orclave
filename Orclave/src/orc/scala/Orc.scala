//
// Orc.scala -- Core traits and classes for the OrcScal DSL
// Project OrcScal
//
// Created by amp
//
// Copyright (c) 2016 The University of Texas at Austin. All rights reserved.
//
// Use and redistribution of this file is governed by the license terms in
// the LICENSE file found in the project's top-level directory and also found at
// URL: http://orc.csres.utexas.edu/license.shtml .
//

package orc.scala

import impl.{ PublicationCont, Counter, Terminator }
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import orc.scala.impl.KilledException
import scala.concurrent.Channel
import scala.annotation.compileTimeOnly

import scala.language.experimental.macros
import scala.language.implicitConversions

// TODO: Once things are looking good set very simply macro requirements: transparent futures 
//       are a first goal, then generating errors for code that drops values in the middle of 
//       Orc code.

// TODO: Add version of otherwise that functions like catch and allows exceptions to be handled. 
//       Normal halts would be given as a HaltedNormally sentinal value that is not of type Exception.

/** An instance of Orc[T] represents an instance of an Orc expression.
  *
  * The expression may or may not be executing already. Other expressions (or
  * external code) can observe publication and halting using `map` and `onHalted`.
  */
trait Orc[+T] {
  /** @group Observation */

  /** Call f and execute the result for each value that this expression
    * publishes.
    *
    * Unlike normal foreach implementations this returns a value. This causes
    * for-loops without yield to return this value and be Orc expressions as
    * expected. However since the loop is not yielding anything the loop will
    * never publish and hense has type Orc[Nothing]. The results of the body
    * expression is ignored even though it may have any publication type.
    *
    * foreach on Orc is similar to `flatMap` because this enables Orc expressions to
    * appear in the body of for-loops over Orc[T]. This should not cause problems
    * because any expression can be wrapped into an Orc[T].
    */
  // TODO: Once other combinators are working, come back to this and see if it is useful.
  //def foreach[B](f: T => Orc[B]): Orc[Nothing]

  /** Call f and execute the result for each value that this expression
    * publishes.
    *
    * map on Orc is actually `flatMap` because this enables Orc expressions to
    * appear in the body of for-loops over Orc[T]. This should not cause problems
    * because any expression can be wrapped into an Orc[T].
    */
  def map[B](f: T ⇒ Orc[B]): Orc[B] = new impl.Branch(this, f)

  /** Call f and execute the result for each value that this expression
    * publishes.
    */
  def flatMap[B](f: T => Orc[B]): Orc[B] = map(f)

  /** Drop publications for which `b` is not true.
    *
    * Only one call to foreach, map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
    */
  def withFilter(b: T => Boolean): Orc[T] = new impl.Filter(this, b)

  /** @group Combinators */

  /** Combine this with `r` such that they will run concurrently.
    */
  def |||[A >: T](r: Orc[A]): Orc[A] = new impl.Parallel(this, r)

  /** Run `f` for each publication of this.
    *
    * @see map
    */
  def branch[B](f: T => Orc[B]): Orc[B] = map(f)

  /** Convert this expression into a future which is bound to the first
    * publication of this.
    */
  def graft: Graft[T] = new impl.Graft(this)

  /** Create a new expression which will kill this when it publishes for the first
    * time.
    */
  def trim: Orc[T] = new impl.Trim(this)

  /** Create an expression which will execute `f` if this never publishes.
    *
    * Run the second operand if the first calls onHalt's f without calling map's f.
    */
  def otherwise[A >: T](f: Orc[A]): Orc[A] = new impl.Otherwise(this, f)

  /** @group Control */

  /** Execute this expression.
    */
  def execute(p: PublicationCont[T])(implicit executor: OrcExecutionContext): Unit
}

trait Graft[+T] {
  def future: Future[T]
  def body: Orc[Nothing]
}

object Orc extends OrcLowPriorityImplicits {
  /** The orc expression which never publishes and halts immediately.
    */
  val stop: Orc[Nothing] = new impl.Stop()

  /** Execute an Orc expression eager/leniently.
    *
    * The returned iterable will contain all the publications of the expressions
    * as they become available. The iterable will end when the Orc expression halts.
    */
  def orclave[T](o: Orc[T])(implicit ctx: OrcExecutionContext): Iterator[T] = {
    // TODO: This is messy. A custom iterator which iteracts directly with the Orc execution would be better.
    val chan = new Channel[Option[T]]()
    val iter = new Iterator[T]() {
      // nextElem has three states: null (load next value), Some(v) (v was published), None (halted)
      var nextElem: Option[T] = null
      def checkNextElem(): Option[T] = nextElem match {
        case null =>
          nextElem = chan.read
          nextElem
        case _ =>
          nextElem
      }
      def hasNext: Boolean = checkNextElem().isDefined
      def next(): T = checkNextElem() match {
        case Some(v) =>
          nextElem = null
          v
        case None =>
          throw new IllegalStateException()
      }
    }
    val newCounter = new impl.CounterNested(ctx.counter, () => chan.write(None))
    try {
      o.execute(v => chan.write(Some(v)))(ctx.withCounter(newCounter))
    } catch {
      case _: KilledException => ()
    } finally {
      // Matched to: initial count in CounterNested above
      newCounter.halt()
    }
    iter
  }

  // TODO: Figure out how to implement an async version of the interface to Orc expressions.
  //       It should enable polling and callbacks and the like. Maybe chained futures or similar.

  /** Get an Orc expression directly without executing it.
    *
    * This triggers Orc macro expansion without executing it or creating an
    * iterable from the publications.
    */
  def orcExpr[T](o: => T): Orc[T] = macro impl.OrcMacro.orcExpr

  /** Alternative syntax for graft.
    */
  def graft[T](f: Orc[T]): Graft[T] = f.graft

  /** Alternative syntax for trim.
    */
  def trim[T](f: Orc[T]): Orc[T] = f.trim

  /** Embed a scala expression in an orclave.
    */
  def scalaclave[T](v: => T): Orc[T] = new impl.ScalaExpr(() => v)

  /** Create an Orc variable expression.
    *
    * The returned Orc expression publishes the value of f once it has one and halts.
    */
  implicit def variable[T](f: Future[T]): Orc[T] = new impl.Variable(f)
}

trait OrcLowPriorityImplicits {
  /** Create an Orc constant expression.
    *
    * The returned Orc expression publishes v and halts.
    */
  implicit def scalaExpr[T](v: => T): Orc[T] = macro impl.OrcMacro.scalaExpr

  @compileTimeOnly("[Orclave] Orc expression can only be used as Orc[T] outside an orclave. An expression was used as T.")
  implicit def orcToBeLifted[T](o: Orc[T]): T = {
    Logger.severe(s"orcToBeLifted should never appear in a compiled program. There is a bug in the macro.\n$o")
    throw new AssertionError("orcToBeLifted should never appear in a compiled program. There is a bug in the macro.")
  }
}
