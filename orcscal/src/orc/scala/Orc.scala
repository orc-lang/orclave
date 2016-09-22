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
    *
    * Only one call to foreach, map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
    */
  // TODO: Once other combinators are working, come back to this and see if it is useful.
  //def foreach[B](f: T => Orc[B]): Orc[Nothing]

  /** Call f and execute the result for each value that this expression
    * publishes.
    *
    * map on Orc is actually `flatMap` because this enables Orc expressions to
    * appear in the body of for-loops over Orc[T]. This should not cause problems
    * because any expression can be wrapped into an Orc[T].
    *
    * Only one call to foreach, map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
    */
  def map[B](f: T ⇒ Orc[B]): Orc[B] = new impl.Branch(this, f)

  /** Call f and execute the result for each value that this expression
    * publishes.
    *
    * Only one call to foreach, map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
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
  // TODO: Nothing cannot be inferred as a type parameter in at least some cases so type inferrence will mess up when using this sometimes.

  /** Execute an Orc expression eager/leniently.
    *
    * The returned iterable will contain all the publications of the expressions
    * as they become available. The iterable will end when the Orc expression halts.
    */
  def orc[T](o: Orc[T])(implicit ctx: OrcExecutionContext): Iterator[T] = {
    val chan = new Channel[Option[T]]()
    val iter = new Iterator[T]() {
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
          nextElem = chan.read
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
      newCounter.halt()
    }
    iter
  }

  // TODO: Figure out how to implement an async version of the interface to Orc expressions.
  //       It should enable polling and callbacks and the like.

  /** Get an Orc expression directly without executing it.
    *
    * This triggers Orc macro expansion without executing it or creating an
    * iterable from the publications.
    */
  def orcRaw[T](o: Orc[T]): Orc[T] = o

  /** Alternative syntax for graft.
    */
  def graft[T](f: Orc[T]): Graft[T] = f.graft

  /** Alternative syntax for trim.
    */
  def trim[T](f: Orc[T]): Orc[T] = f.trim

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
  implicit def scalaExpr[T](v: => T): Orc[T] = new impl.ScalaExpr(() => v)
}
