package orc.scala.impl

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.atomic.AtomicInteger

object FutureUtil {
  def tuple[T1, T2](f1: Future[T1], f2: Future[T2])(implicit executor: ExecutionContext): Future[(T1, T2)] = {
    val promise = Promise[(T1, T2)]()
    val remaining = new AtomicInteger(2)
    @volatile
    var v1: T1 = null.asInstanceOf[T1]
    @volatile
    var v2: T2 = null.asInstanceOf[T2]
    
    def maybeComplete() = {
      if(remaining.decrementAndGet() == 0) {
        promise.success((v1, v2))
      }
    }
    
    f1.onComplete { 
      case Success(v) =>
        v1 = v
        maybeComplete()
      case Failure(e) =>
        promise.tryFailure(e)
    }
    f2.onComplete { 
      case Success(v) =>
        v2 = v
        maybeComplete()
      case Failure(e) =>
        promise.tryFailure(e)
    }
    promise.future
  }
  def tuple[T1, T2, T3](f1: Future[T1], f2: Future[T2], f3: Future[T3])(implicit executor: ExecutionContext): Future[(T1, T2, T3)] = {
    val promise = Promise[(T1, T2, T3)]()
    val remaining = new AtomicInteger(3)
    @volatile
    var v1: T1 = null.asInstanceOf[T1]
    @volatile
    var v2: T2 = null.asInstanceOf[T2]
    @volatile
    var v3: T3 = null.asInstanceOf[T3]
    
    def maybeComplete() = {
      if(remaining.decrementAndGet() == 0) {
        promise.success((v1, v2, v3))
      }
    }
    
    f1.onComplete { 
      case Success(v) =>
        v1 = v
        maybeComplete()
      case Failure(e) =>
        promise.tryFailure(e)
    }
    f2.onComplete { 
      case Success(v) =>
        v2 = v
        maybeComplete()
      case Failure(e) =>
        promise.tryFailure(e)
    }
    f3.onComplete { 
      case Success(v) =>
        v3 = v
        maybeComplete()
      case Failure(e) =>
        promise.tryFailure(e)
    }
    promise.future
  }
  def tuple[T1, T2, T3, T4](f1: Future[T1], f2: Future[T2], f3: Future[T3], f4: Future[T4])(implicit executor: ExecutionContext): Future[(T1, T2, T3, T4)] = {
    ???
  }
}