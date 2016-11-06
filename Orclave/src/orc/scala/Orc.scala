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

// TODO: Add direct implementations of convience combinators.

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

  /** Run `f` after this halts. Ignore publications of this.
    */
  def andthen[B](f: Orc[B]): Orc[B] = silent.otherwise(f)

  /** Ignore publications of this.
    */
  def silent: Orc[Nothing] = map(_ => Orc.stop)

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
    * The expression must already be an Orc[T] object.
    * 
    * The returned iterable will contain all the publications of the expressions
    * as they become available. The iterable will end when the Orc expression halts.
    */
  def orcToBlockingIterable[T](o: Orc[T])(implicit ctx: OrcExecutionContext): Iterator[T] = {
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
  
  /** Execute an Orc expression eager/leniently.
    *
    * The returned iterable will contain all the publications of the expressions
    * as they become available. The iterable will end when the Orc expression halts.
    */
  def orclave[T, R](o: T)(implicit ctx: OrcExecutionContext, evidence: StripOrcConstructor[T, R]): Iterator[R] = macro impl.OrcMacro.orclave

  /** Get an Orc expression directly without executing it.
    *
    * This triggers Orc macro expansion without executing it or creating an
    * iterable from the publications.
    */
  def orcExpr[T, R](o: => T)(implicit evidence: StripOrcConstructor[T, R]): Orc[R] = macro impl.OrcMacro.orcExpr

  /** Alternative syntax for graft.
    */
  def graft[T](f: Orc[T]): Graft[T] = f.graft

  /** Alternative syntax for trim.
    */
  def trim[T](f: Orc[T]): Orc[T] = f.trim

  /** Alternative syntax for silent.
    */
  def silence[T](f: Orc[T]): Orc[Nothing] = f.silent

  /** Embed a scala expression in an orclave.
    */
  def scalaclave[T](v: => T): Orc[T] = new impl.ScalaExpr(() => v)

  /** Create an Orc variable expression.
    *
    * The returned Orc expression publishes the value of f once it has one and halts.
    */
  def variable[T](f: Future[T]): Orc[T] = new impl.Variable(f)

  /** Create an Orc constant expression.
    *
    * The returned Orc expression publishes v and halts.
    */
  def scalaExpr[T](v: => T): Orc[T] = new impl.ScalaExpr(() => v)


  // Integration of Orc with normal Scala types
  @compileTimeOnly("[Orclave] Orc expression can only be used as their underlying type inside an orclave. An Orc expression was used as T.")
  implicit def orcInScalaContext[T](o: Orc[T]): T = {
    throw new AssertionError("orcInScalaContext should never appear in a compiled program. There is a bug in the macro.")
  }
  
  @compileTimeOnly("[Orclave] Orc operators can only be used on arbitrary values inside an orclave.")
  implicit def scalaInOrcContext[T](o: T): Orc[T] = {
    throw new AssertionError("scalaInOrcContext should never appear in a compiled program. There is a bug in the macro.")
  }  

  // TODO: Consider lifting these conversions to (especially future->scala) to a separate import. So as to avoid confusion who are not bringing along their own futures.
  // Integration of Futures with Orc and Scala types
  @compileTimeOnly("[Orclave] Orc Future[T] conversions can only be used inside an orclave. This is probably caused by importing Orc._ implicits outside an Orclave.")
  implicit def futureInScalaContext[T](o: Future[T]): T = {
    throw new AssertionError("futureInScalaContext should never appear in a compiled program. There is a bug in the macro.")
  }
  
  @compileTimeOnly("[Orclave] Orc Future[T] conversions can only be used inside an orclave. This is probably caused by importing Orc._ implicits outside an Orclave.")
  implicit def futureInOrcContext[T](o: Future[T]): Orc[T] = {
    throw new AssertionError("futureInOrcContext should never appear in a compiled program. There is a bug in the macro.")
  }
}

trait OrcLowPriorityImplicits {
  // Integration of Futures with normal Scala types
  @compileTimeOnly("[Orclave] Orc operators can only be used on arbitrary values inside an orclave.")
  implicit def scalaInFutureContext[T](o: T): Future[T] = {
    throw new AssertionError("scalaInFutureContext should never appear in a compiled program. There is a bug in the macro.")
  }

  sealed trait StripOrcConstructor[T, R]

  object StripOrcConstructor extends StripOrcConstructorLowPriority {
    @compileTimeOnly("[Orclave] ICE: Bug in Orclave macro.")
    implicit def OrcEvidence[T]: StripOrcConstructor[Orc[T], T] = null
    @compileTimeOnly("[Orclave] ICE: Bug in Orclave macro.")
    implicit def FutureEvidence[T]: StripOrcConstructor[Future[T], T] = null
  }

  trait StripOrcConstructorLowPriority {
    @compileTimeOnly("[Orclave] ICE: Bug in Orclave macro.")
    implicit def ScalaEvidence[T]: StripOrcConstructor[T, T] = null
  }
}


