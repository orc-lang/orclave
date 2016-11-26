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

// TODO: Expressions are not always terminating properly after a trim.

class Parallel[T](l: Orc[T], r: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    ctx.schedule { l.execute(p) }
    ctx.schedule { r.execute(p) }
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
    try {
      l.execute(v => {
        hasPublished.set(true)
        p(v)
      })(ctx.withCounter(newCounter))
    } finally {
      // Matched against: initial count
      newCounter.halt()
    }
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

class Graft[T](e: Orc[T]) extends orc.scala.Graft[T] with Terminatable {
  val promise = Promise[T]()

  def future = promise.future

  val body: Orc[Nothing] = new Orc[Nothing] {
    def execute(p: PublicationCont[Nothing])(implicit ctx: OrcExecutionContext): Unit = {
      def onHalt() = {
        promise.tryFailure(HaltException.SINGLETON)
        ctx.terminator.removeChild(Graft.this)
      }
      val newCounter = new CounterNested(ctx.counter, onHalt)
      // TODO: Check
      try {
        ctx.terminator.addChild(Graft.this)
        e.execute(promise.trySuccess)(ctx.withCounter(newCounter))
      } finally {
        // Matched against: initial count
        newCounter.halt()
      }
    }
  }
  
  def kill(): Unit = {
    promise.tryFailure(KilledException.SINGLETON)
  }
}

class ScalaExpr[T](v: () => T) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    ctx.schedule {
      ctx.enterTerminatable()
      val res = try {
        Some(v())
      } catch {
        case _: Exception =>
          None
      } finally {
        ctx.leaveTerminatable()
      }
      res.map(p)
    }
  }
}

class Variable[T](fut: Future[T]) extends Orc[T] {
  def execute(p: PublicationCont[T])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
    // Matched against: halt in onComplete cases
    ctx.prepareSpawn()
    fut.onComplete {
      case Success(x) =>
        try {
          try {
            p(x)
          } finally {
            // Matched against: prepareSpawn before onComplete call 
            ctx.halt()
          }
        } catch {
          case _: KilledException => ()
        }
      case Failure(e) =>
        try {
          // Matched against: prepareSpawn before onComplete call 
          ctx.halt()
        } catch {
          case _: KilledException => ()
        }
    }
  }
}

class Stop() extends Orc[Nothing] {
  def execute(p: PublicationCont[Nothing])(implicit ctx: OrcExecutionContext): Unit = {
    // TODO: Check
  }
}
