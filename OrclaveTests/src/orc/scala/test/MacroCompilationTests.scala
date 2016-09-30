package orc.scala.test

import orc.scala._
import Orc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object MacroCompilationTests {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)
  
  def site(): Orc[String] = scalaExpr("test")

  def o0 = orclave {
    orcExpr {
      val x = println("a")
      //println(x)
      val y = println("b")
      val z = 42
      //def f() = 3
      site().startsWith("t")
    }
  }
}