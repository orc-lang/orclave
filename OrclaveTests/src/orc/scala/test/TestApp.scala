package orc.scala.test

import orc.scala.OrcExecutionContext
import orc.scala.Orc
import orc.scala.Orc._
import scala.concurrent.ExecutionContext

object TestApp {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  def badSleep(n: Long): Orc[Unit] = scalaExpr(Thread.sleep(n))

  def delayedValue[T](n: Long, v: T): T = { Thread.sleep(n); v }
  def delayedValueO[T](n: Long, v: T): Orc[T] = scalaExpr { Thread.sleep(n); v }

  def main(args: Array[String]): Unit = {
    val o = orcExpr[Int] {
      //val a = delayedValue(1000, 1)
      val b = scalaExpr(delayedValue(1000, 2))
      //val c = delayedValue(1000, 3)
      scalaExpr(orcToBeLifted(b) + orcToBeLifted(b)) // + a + c + scalaclave(10)
    }
    val r = orclave { o }
    println(r.toList)
  }
}