//
// Impl.scala -- Orc Combinator implementations
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

package orc.scala.impl

import orc.scala._
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Success
import scala.util.Failure

class Parallel[T](l: Orc[T], r: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    ctx.schedule { l.execute(p) }
    r.execute(p)
  }
}

class Filter[T](e: Orc[T], pred: T ⇒ Boolean) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    e.execute(x => {
      if (pred(x))
        p(x)
    })
  }
}

class Branch[A, B](l: Orc[A], r: A ⇒ Orc[B]) extends Orc[B] {
  def execute(p: PublicationCont[B])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    l.execute(x => {
      ctx.schedule { r(x).execute(p) }
    })
  }
}

class Otherwise[T](l: Orc[T], r: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    val hasPublished = new AtomicBoolean(false)
    def onHalt() = {
      if (!hasPublished.get)
        ctx.schedule {
          r.execute(p)
        }
    }
    val newCounter = new CounterNested(ctx.counter, onHalt)

    // TODO: check
    l.execute(v => {
      hasPublished.set(true)
      p(v)
    })(ctx.withCounter(newCounter))
    newCounter.halt()
  }
}

class Trim[T](e: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    val newTerminator = new Terminator()

    // TODO: check
    e.execute(v => {
      newTerminator.kill()
      p(v)
    })(ctx.withTerminator(newTerminator))
  }
}

class Graft[T](e: Orc[T]) extends orc.scala.Graft[T] {
  val promise = Promise[T]()

  def future = promise.future

  val body: Orc[Nothing] = new Orc[Nothing] {
    def execute(p: PublicationCont[Nothing])(implicit ctx: OrcExecutionContext): Unit = {
      def onHalt() = {
        promise.tryFailure(HaltException.SINGLETON)
      }
      val newCounter = new CounterNested(ctx.counter, onHalt)
      // TODO: Check
      e.execute(promise.success)(ctx.withCounter(newCounter))
      newCounter.halt()
    }
  }
}

class ScalaExpr[T](v: () => T) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    ctx.schedule {
      val res = try {
        Some(v())
      } catch {
        case _: Exception =>
          None
      }
      res.map(p)
    }
  }
}

class Variable[T](fut: Future[T]) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    ctx.counter.prepareSpawn()
    fut.onComplete {
      case Success(x) =>
        p(x)
        ctx.counter.halt()
      case Failure(e) =>
        ctx.counter.halt()
    }
  }
}

class Stop() extends Orc[Nothing] {
  def execute(p: PublicationCont[Nothing])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
  }
}
