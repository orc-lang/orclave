package orc.scala.test

import orc.scala._

import Orc._
import Future._

object CompilationTests {
  val o0: Iterable[Int] = orc { 1 }

  val o1: Iterable[Unit] = orc {
    for (x <- (1 ||| 2)) yield {
      println(x)
    }
  }

  val o2: Iterable[Int] = orc {
    val x = (42).graft
    x
  }

  val o3: Iterable[Nothing] = orc {
    val x = (42).graft
    variable(x).flatMap[Nothing](_ => stop)
  }

  def Site(): Orc[Int] = 42
  def Other(): Orc[String] = "test"

  val o4: Iterable[String] = orc {
    val x = (42).graft
    for(_ <- variable(x)) yield Other()
  }


  val big0: Iterable[Unit] = orc {
    {
      val x: Future[Int] = graft { trim { Site() } }
      for {
        z <- (for ((x_, y_) <- join(x, Other().graft)) yield x_ + y_) |||
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