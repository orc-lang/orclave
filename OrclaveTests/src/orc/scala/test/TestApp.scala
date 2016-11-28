package orc.scala.test

import orc.scala.OrcExecutionContext
import orc.scala.Orc
import orc.scala.Orc._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import orc.scala.impl.FutureUtil
import java.util.Arrays

object TestApp {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  def badSleep(n: Long): Orc[Unit] = scalaExpr(Thread.sleep(n))

  def delayedValue[T](n: Long, v: T): T = { Thread.sleep(n); v }
  def delayedValueO[T](n: Long, v: T): Orc[T] = scalaExpr { Thread.sleep(n); v }

  def takeFuture[T](f: Future[T]): Unit = println(f)

  def methadd1(n: Int) = n + 1
  def add2(n: Int)(m: Int) = n + m

  def main(args: Array[String]): Unit = {
    val r = orclave {
      List(1, 2)
    }

    for (p <- r) {
      println(s"Pub: $p")
    }
  }
}

object TestObj {
  var x = 0
}