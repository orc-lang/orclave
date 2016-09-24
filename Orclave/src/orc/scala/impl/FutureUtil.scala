package orc.scala.impl

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object FutureUtil {
  def tuple[T1](f1: Future[T1])(implicit executor: ExecutionContext): Future[Tuple1[T1]] = {
    ???
  }
  def tuple[T1, T2](f1: Future[T1], f2: Future[T2])(implicit executor: ExecutionContext): Future[(T1, T2)] = {
    ???
  }
  def tuple[T1, T2, T3](f1: Future[T1], f2: Future[T2], f3: Future[T3])(implicit executor: ExecutionContext): Future[(T1, T2, T3)] = {
    ???
  }
  def tuple[T1, T2, T3, T4](f1: Future[T1], f2: Future[T2], f3: Future[T3], f4: Future[T4])(implicit executor: ExecutionContext): Future[(T1, T2, T3, T4)] = {
    ???
  }
}