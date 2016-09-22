package orc.scala.test

import orc.scala._

import Orc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object CompilationTests {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  def o0: Iterator[Int] = orc { 1 }

  def o1: Iterator[Unit] = orc {
    for (x <- (1 ||| 2)) yield {
      println(x)
    }
  }

  def o2: Iterator[Int] = orc {
    val x = (42).graft
    x.future
  }

  def o3: Iterator[Nothing] = orc {
    val x = (42).graft
    x.body |||
      (for (_ <- variable(x.future)) yield { stop })
  }

  def Site(): Orc[Int] = 42
  def Other(): Orc[String] = "test"

  def o4: Iterator[String] = orc {
    val x = (42).graft
    for (_ <- variable(x.future)) yield Other()
  }

  def big0: Iterator[Unit] = orc {
    {
      val x: Graft[Int] = graft { trim { Site() } }
      for {
        z <- (for (Seq(x_ : Int, y_ : String) <- Future.sequence(Seq(x.future, Other().graft.future))) yield x_ + y_) |||
          x.toString
        _ <- println(z)
      } yield {
        ()
      }
    } otherwise {
      println("Done")
    }
  }
}