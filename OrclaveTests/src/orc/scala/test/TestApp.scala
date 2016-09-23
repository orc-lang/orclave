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
    val r = orclave {
      orcExpr {
        val a = delayedValue(1000, 1)
        val b = delayedValueO(1000, 2)
        val c = delayedValue(1000, 3)
        a + b + c
      }
    }
    println(r.toList)
  }
}