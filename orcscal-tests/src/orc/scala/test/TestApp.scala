package orc.scala.test

import orc.scala.OrcExecutionContext
import orc.scala.Orc
import orc.scala.Orc._
import scala.concurrent.ExecutionContext

object TestApp {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  def badSleep(n: Long): Orc[Unit] = scalaExpr(Thread.sleep(n))

  def main(args: Array[String]): Unit = {
    Util.timeIt {
      val r = orc {
        (for (_ <- badSleep(100) ||| badSleep(100) ||| badSleep(100)) yield {
          1
        }) ||| {
          (for (_ <- badSleep(200)) yield {
            5
          })
        }
      }
      println(r.toList)
    }
  }
}