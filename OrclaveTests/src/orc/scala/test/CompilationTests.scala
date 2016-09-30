package orc.scala.test

import orc.scala._
import Orc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object CompilationTests {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)
  
  // TODO: This is just random stuff. Make a principled test set.
  
  // TODO: Add compilation failure tests
  // def x = site() + 1

  def o0: Iterator[Int] = orclave { 1 }

  def o1: Iterator[Unit] = orclave {
    for (x <- (1 ||| 2)) yield {
      println(x)
    }
  }

  def o2: Iterator[Int] = orclave {
    val x = 42
    x
  }

  def o3: Iterator[Nothing] = orclave {
    val x = 42
    for (_ <- x) yield { stop }
  }

  def Site(): Orc[Int] = orcExpr(42)
  def Other(): Orc[String] = orcExpr("test")

  def o4: Iterator[String] = orclave {
    val x = 42
    for (_ <- x) yield Other()
  }
  
  def Site2(): Orc[String] = scalaExpr("test")

  def o5 = orclave {
    val x = println("a")
    //println(x)
    val y = println("b")
    val z = 42
    //def f() = 3
    Site2().startsWith("t")
  }
  
  def big0: Iterator[Unit] = orclave {
    {
      val x = trim { Site() }
      for {
        z <- (x + Other()) ||| x.toString
        _ <- println(z)
      } yield {
        stop
      }
    } otherwise {
      println("Done")
    }
  }
}