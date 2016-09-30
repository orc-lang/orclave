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

  def methadd1(n: Int) = n + 1
  def add2(n: Int)(m: Int) = n + m

  def main(args: Array[String]): Unit = {
    def x = 1
    def add1(n: Int) = n + 1
    val f = Future.successful(1)
    val o1 = orcExpr(1)
    val o2 = orcExpr(o1)
    val o3 = orcExpr(f)
    val o = orcExpr {
      add1(1) |||
        methadd1(1) |||
        {
          (for (_ <- (for (n <- delayedValueO(500, 3) ||| delayedValue(1000, 4) ||| 5 ||| f ||| x) yield println(s"Print: ${(42 + n + f)}"))) yield stop) otherwise
            5
        }
      //add2(1)(add1(1))
    }
    val r = orclave { o }
    for (p <- r) {
      println(s"Pub: $p")
    }
  }
}