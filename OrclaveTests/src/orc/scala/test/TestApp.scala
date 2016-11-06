package orc.scala.test

import orc.scala.OrcExecutionContext
import orc.scala.Orc
import orc.scala.Orc._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import orc.scala.impl.FutureUtil

object TestApp {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  def badSleep(n: Long): Orc[Unit] = scalaExpr(Thread.sleep(n))

  def delayedValue[T](n: Long, v: T): T = { Thread.sleep(n); v }
  def delayedValueO[T](n: Long, v: T): Orc[T] = scalaExpr { Thread.sleep(n); v }

  def takeFuture[T](f: Future[T]): Unit = println(f)

  def methadd1(n: Int) = n + 1
  def add2(n: Int)(m: Int) = n + m

  def ift(b: Boolean): Orc[Unit] = if(b) scalaclave(()) else stop

  def main(args: Array[String]): Unit = {
    //val f = Future.successful(1)
    /*def x = scalaclave { 1 }
    def add1(n: Int) = n + 1
    val o1 = orcExpr(1)
    val o2 = orcExpr(o1)
    val o3 = orcExpr(f)
    */
    val r = orclave {
      def f(t: Int, x: Int): String = for (_ <- badSleep(t); _ <- ift(x < 3)) yield {
        x.toString() ||| f(t, x + 1)
      }
      f(100, 1)
    }

    for (p <- r) {
      println(s"Pub: $p")
    }
  }
}

object TestObj {
  var x = 0
}