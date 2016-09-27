package orc.scala.test

import org.scalatest._
import org.scalatest.concurrent.TimeLimits._
import org.scalatest.time.SpanSugar._

import orc.scala._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class OrcRuntimeTests extends FlatSpec with Matchers {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)
  
  
  import Orc.{scalaclave, scalaExpr, orclave, stop, variable, trim, graft}
  
  import scala.language.implicitConversions
  implicit def scalaToOrc[T](v: => T): Orc[T] = scalaExpr(v) 

  def badSleep(n: Long): Orc[Unit] = scalaclave(Thread.sleep(n))

  "Orclave runtime" should "execute constant expressions" in {
    val r = orclave { scalaExpr(1) }
    r.toList should be(List(1))
  }

  it should "execute parallel constants" in {
    val r = orclave { scalaExpr(1) ||| scalaExpr(2) }
    r.toList should contain theSameElementsAs (List(1, 2))
  }

  it should "execute stop" in {
    val r = orclave { stop }
    r.toList.size should be(0)
  }

  it should "execute side-effect at the correct time" in {
    var x = 0
    val o = scalaExpr { x = 42 }
    x should be(0)
    val r = orclave(o)
    r.toSet should be(Set(()))
    x should be(42)
  }

  it should "execute graft" in {
    failAfter(10 seconds) {
      val r = orclave {
        val x = (42).graft
        x.body |||
          variable(x.future)
      }
      r.toList should be(List(42))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val x = (stop).graft
        x.body |||
          variable(x.future)
      }
      r.toList should be(List())
    }
  }

  it should "execute otherwise" in {
    failAfter(10 seconds) {
      val r = orclave {
        stop otherwise scalaExpr(42)
      }
      r.toList should be(List(42))
    }
    failAfter(10 seconds) {
      val r = orclave {
        5 otherwise scalaExpr(42)
      }
      r.toList should be(List(5))
    }
  }

  it should "execute trim" in {
    failAfter(10 seconds) {
      val r = orclave {
        trim { 1 ||| scalaExpr(2) }
      }
      val s = r.toList
      s.size should be(1)
      s should contain oneOf (1, 2)
    }
    failAfter(10 seconds) {
      val r = orclave {
        trim { scalaclave({ Thread.sleep(100); 1 }) ||| scalaExpr(2) }
      }
      val s = r.toList
      s.size should be(1)
      s should contain(2)
    }
    failAfter(10 seconds) {
      val r = orclave {
        trim { 1 ||| scalaclave({ Thread.sleep(100); 2 }) }
      }
      val s = r.toList
      s.size should be(1)
      s should contain(1)
    }
  }

  it should "execute branch" in {
    failAfter(10 seconds) {
      val r = orclave {
        for (x <- 3) yield {
          scalaExpr(x + 1)
        }
      }
      r.toList should contain theSameElementsAs List(4)
    }
    failAfter(10 seconds) {
      val r = orclave {
        for (x <- 1 ||| scalaExpr(2)) yield {
          scalaExpr(x + 1)
        }
      }
      r.toList should contain theSameElementsAs List(2, 3)
    }
  }

  it should "execute branch on future" in {
    failAfter(10 seconds) {
      val r = orclave {
        val x = badSleep(100).graft
        x.body ||| scalaExpr(1) ||| {
          for (_ <- variable(x.future)) yield {
            scalaExpr(4)
          }
        }
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val x = badSleep(100).graft
        x.body ||| {
          for (_ <- variable(x.future)) yield {
            scalaExpr(4)
          }
        } ||| scalaExpr(1)
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val x = badSleep(100).graft
        (
          for (_ <- variable(x.future)) yield {
            scalaExpr(4)
          }) ||| scalaExpr(1) ||| x.body
      }
      r.toList should be(List(1, 4))
    }
  }

  it should "execute branch with sleep" in {
    failAfter(10 seconds) {
      val r = orclave {
        1 ||| {
          for (_ <- badSleep(100)) yield {
            scalaExpr(4)
          }
        }
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orclave {
        {
          for (_ <- badSleep(100)) yield {
            scalaExpr(4)
          }
        } ||| scalaExpr(1)
      }
      r.toList should be(List(1, 4))
    }
  }

  it should "execute otherwise with sleep" in {
    failAfter(10 seconds) {
      val r = orclave {
        1 ||| {
          (for (_ <- badSleep(100)) yield {
            stop
          }) otherwise scalaExpr(3)
        }
      }
      r.toList should be(List(1, 3))
    }
    failAfter(10 seconds) {
      val r = orclave {
        {
          (for (_ <- badSleep(100)) yield {
            stop
          }) otherwise scalaExpr(3)
        } ||| scalaExpr(1)
      }
      r.toList should be(List(1, 3))
    }
  }

  it should "execute parallel with sleep" in {
    failAfter(10 seconds) {
      val r = orclave {
        (for (_ <- badSleep(100) ||| badSleep(100) ||| badSleep(100)) yield {
          scalaExpr(1)
        }) ||| {
          (for (_ <- badSleep(200)) yield {
            scalaExpr(5)
          })
        }
      }
      r.toList should be(List(1, 1, 1, 5))
    }
    failAfter(10 seconds) {
      val r = orclave {
        {
          (for (_ <- badSleep(200)) yield {
            scalaExpr(5)
          })
        } |||
          (for (_ <- badSleep(100) ||| badSleep(100) ||| badSleep(100)) yield {
            scalaExpr(1)
          })
      }
      r.toList should be(List(1, 1, 1, 5))
    }
  }

  it should "execute trim with sleep" in {
    failAfter(10 seconds) {
      var x = 0
      val r = orclave {
        trim {
          1 ||| {
            for (_ <- badSleep(100)) yield {
              scalaExpr(x = 1)
            }
          }
        }
      }
      x should be(0)
      r.toList should be(List(1))
      x should be(0)
    }
    failAfter(10 seconds) {
      var x = 0
      val r = orclave {
        trim {
          {
            (for (_ <- badSleep(100)) yield {
              scalaExpr(x = 1)
            })
          } ||| scalaExpr(1)
        }
      }
      x should be(0)
      r.toList should be(List(1))
      x should be(0)
    }
    failAfter(10 seconds) {
      var x = 0
      val r = orclave {
        trim {
          {
            (for (_ <- badSleep(200)) yield {
              scalaExpr(x = 1)
            })
          } |||
            {
              (for (_ <- badSleep(100)) yield {
                scalaExpr(1)
              })
            }
        }
      }
      x should be(0)
      r.toList should be(List(1))
      x should be(0)
    }
  }

  it should "kill scala sleep in trim" in {
    failAfter(10 seconds) {
      val r = orclave {
        ({
          for (
            _ <- trim {
              () ||| badSleep(1000)
            }
          ) yield stop
        } otherwise {
          scalaExpr(1)
        }) ||| (for (_ <- badSleep(100)) yield {
          scalaExpr(2)
        })
      }
      r.toList should be(List(1, 2))
    }
  }

  it should "interrupt scala code in trim" in {
    failAfter(10 seconds) {
      var exc: Exception = null
      val r = orclave {
        trim {
          (for (_ <- badSleep(100)) yield scalaExpr(())) ||| scalaclave {
            val o = new Object()
            try {
              o.synchronized { o.wait() }
            } catch {
              case e: Exception =>
                exc = e
            }
          }
        }
      }
      r.toList should be(List(()))
      exc shouldBe a[InterruptedException]
    }
    failAfter(10 seconds) {
      var interrupted = false
      val r = orclave {
        trim {
          (for (_ <- badSleep(100)) yield scalaExpr(())) ||| scalaclave {
            while (!interrupted) {
              interrupted = Thread.interrupted()
            }
          }
        }
      }
      r.toList should be(List(()))
      interrupted should be(true)
    }
  }
}