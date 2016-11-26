package orc.scala.test

import org.scalatest._
import org.scalatest.concurrent.TimeLimits._
import org.scalatest.time.SpanSugar._

import orc.scala._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class OrclaveRuntimeTests extends FlatSpec with Matchers {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  import Orc._

  def badSleep(n: Long): Orc[Unit] = scalaclave(Thread.sleep(n))
  def ift(b: Boolean): Orc[Unit] = if(b) scalaclave(()) else stop
  def delayedValue[T](n: Long, v: T): T = { Thread.sleep(n); v }

  "Orclave (macro code) runtime" should "execute constant expressions" in {
    val r = orclave { 1 }
    r.toList should be(List(1))
  }

  it should "execute parallel constants" in {
    val r = orclave { 1 ||| 2 }
    r.toList should contain theSameElementsAs (List(1, 2))
  }

  it should "execute stop" in {
    val r = orclave { stop }
    r.toList.size should be(0)
  }

  it should "execute side-effect at the correct time" in {
    var x = 0
    val o = scalaclave { x = 42 }
    x should be(0)
    val r = orclave(o)
    r.toSet should be(Set(()))
    x should be(42)
  }

  it should "execute graft" in {
    failAfter(10 seconds) {
      val r = orclave {
        val x = 42
        x
      }
      r.toList should be(List(42))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val x = stop
        x
      }
      r.toList should be(List())
    }
  }

  it should "execute otherwise" in {
    failAfter(10 seconds) {
      val r = orclave {
        stop otherwise 42
      }
      r.toList should be(List(42))
    }
    failAfter(10 seconds) {
      val r = orclave {
        5 otherwise 42
      }
      r.toList should be(List(5))
    }
  }

  it should "execute trim" in {
    failAfter(10 seconds) {
      val r = orclave {
        trim { 1 ||| 2 }
      }
      val s = r.toList
      s.size should be(1)
      s should contain oneOf (1, 2)
    }
    failAfter(10 seconds) {
      val r = orclave {
        trim { delayedValue(100, 1) ||| 2 }
      }
      val s = r.toList
      s.size should be(1)
      s should contain(2)
    }
    failAfter(10 seconds) {
      val r = orclave {
        trim { 1 ||| delayedValue(100, 2) }
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
          x + 1
        }
      }
      r.toList should contain theSameElementsAs List(4)
    }
    failAfter(10 seconds) {
      val r = orclave {
        for (x <- 1 ||| 2) yield {
          x + 1
        }
      }
      r.toList should contain theSameElementsAs List(2, 3)
    }
  }

  it should "execute branch on future" in {
    failAfter(10 seconds) {
      val r = orclave {
        val x = badSleep(100)
        1 ||| {
          for (_ <- x) yield {
            4
          }
        }
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val x = badSleep(100)

        {
          for (_ <- x) yield {
            4
          }
        } ||| 1
      }
      r.toList should be(List(1, 4))
    }
  }

  it should "execute branch with sleep" in {
    failAfter(10 seconds) {
      val r = orclave {
        1 ||| {
          for (_ <- badSleep(100)) yield {
            4
          }
        }
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orclave {
        {
          for (_ <- badSleep(100)) yield {
            4
          }
        } ||| 1
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
          }) otherwise 3
        }
      }
      r.toList should be(List(1, 3))
    }
    failAfter(10 seconds) {
      val r = orclave {
        {
          (for (_ <- badSleep(100)) yield {
            stop
          }) otherwise 3
        } ||| 1
      }
      r.toList should be(List(1, 3))
    }
  }

  it should "execute parallel with sleep" in {
    failAfter(10 seconds) {
      val r = orclave {
        (for (_ <- badSleep(100) ||| badSleep(100) ||| badSleep(100)) yield {
          1
        }) ||| {
          (for (_ <- badSleep(200)) yield {
            5
          })
        }
      }
      r.toList should be(List(1, 1, 1, 5))
    }
    failAfter(10 seconds) {
      val r = orclave {
        {
          (for (_ <- badSleep(200)) yield {
            5
          })
        } |||
          (for (_ <- badSleep(100) ||| badSleep(100) ||| badSleep(100)) yield {
            1
          })
      }
      r.toList should be(List(1, 1, 1, 5))
    }
  }

  it should "execute trim with sleep" in {
    failAfter(10 seconds) {
      var x = 0
      def f() = scalaclave { x = 1 }
      val r = orclave {
        trim {
          1 ||| {
            for (_ <- badSleep(100)) yield {
              f()
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
      def f = scalaclave { x = 1 }
      val r = orclave {
        trim {
          {
            (for (_ <- badSleep(100)) yield {
              f
            })
          } ||| 1
        }
      }
      x should be(0)
      r.toList should be(List(1))
      x should be(0)
    }
    failAfter(10 seconds) {
      var x = 0
      def f = scalaclave { x = 1 }
      val r = orclave {
        trim {
          {
            (for (_ <- badSleep(200)) yield {
              f
            })
          } |||
            {
              (for (_ <- badSleep(100)) yield {
                1
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
          1
        }) ||| (for (_ <- badSleep(100)) yield {
          2
        })
      }
      r.toList should be(List(1, 2))
    }
  }

  it should "interrupt scala code in trim" in {
    failAfter(10 seconds) {
      var exc: Exception = null
      def f1 = {
        scalaclave {
          val o = new Object()
          try {
            o.synchronized { o.wait() }
          } catch {
            case e: Exception =>
              exc = e
          }
        }
      }
      exc shouldBe null
      val r = orclave {
        trim {
          (for (_ <- badSleep(100)) yield ()) ||| f1
        }
      }
      r.toList should be(List(()))
      // Sleep a little to make sure the kill actually happened.
      Thread.sleep(100)
      exc shouldBe a[InterruptedException]
    }
    failAfter(10 seconds) {
      var interrupted = false
      def f2 = {
        scalaclave {
          while (!interrupted) {
            interrupted = Thread.interrupted()
          }
        }
      }
      interrupted should be(false)
      val r = orclave {
        trim {
          (for (_ <- badSleep(100)) yield ()) ||| f2
        }
      }
      r.toList should be(List(()))
      // Sleep a little to make sure the kill actually happened.
      Thread.sleep(100)
      interrupted should be(true)
    }
  }

  it should "support simple scalaclaves" in {
    failAfter(10 seconds) {
      var exc: Exception = null
      exc shouldBe null
      val r = orclave {
        trim {
          (for (_ <- badSleep(100)) yield ()) ||| scalaclave {
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
      // Sleep a little to make sure the kill actually happened.
      Thread.sleep(100)
      exc shouldBe a[InterruptedException]
    }

    failAfter(10 seconds) {
      var x = 0
      val r = orclave {
        trim {
          {
            (for (_ <- badSleep(200)) yield {
              scalaclave { x = 1 }
            })
          } |||
            {
              (for (_ <- badSleep(100)) yield {
                1
              })
            }
        }
      }
      x should be(0)
      r.toList should be(List(1))
      x should be(0)
    }
  }

  it should "support assignment" in {
    failAfter(10 seconds) {
      var x = 0
      val r = orclave {
        for (_ <- badSleep(200)) yield {
          x = 1
        }
      }
      x should be(0)
      r.toList should be(List(()))
      x should be(1)
    }
  }

  it should "support vars" in {
    failAfter(10 seconds) {
      val r = orclave {
        var x = 0
        val _ = for (_ <- badSleep(200)) yield {
          x = 1
        }
        x |||
          (for (_ <- badSleep(300)) yield {
            x
          })
      }
      r.toList should be(List(0, 1))
    }
  }
  
  it should "support defs" in {
    failAfter(10 seconds) {
      val r = orclave {
        def f(t: Int, x: Int): String = for (_ <- badSleep(t)) yield {
          x.toString
        }
        f(100, 1) ||| f(0, 2)
      }
      r.toList should be(List("2", "1"))
    }
  }
  
  it should "support recursive defs" in {
    failAfter(10 seconds) {
      val r = orclave {
        def f(t: Int, x: Int): String = for (_ <- badSleep(t); _ <- ift(x < 3)) yield {
          x.toString() ||| f(t, x+1)
        }
        f(100, 1)
      }
      r.toList should be(List("1", "2"))
    }
  }
  
  it should "trim works on recursive defs" in {
    failAfter(10 seconds) {
      val r = orclave {
        def f(t: Int, x: Int): String = for (_ <- badSleep(t)) yield {
          x.toString() ||| f(t, x+1)
        }
        trim {
          f(100, 1)
        }
      }
      r.toList should be(List("1"))
    }
  }
  
  it should "support if statements" in {
    failAfter(10 seconds) {
      val b = true
      val r = orclave {
        if(b) 1 else 2
      }
      r.toList should be(List(1))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val b = false
        if(b) 1 else 2
      }
      r.toList should be(List(2))
    }
    failAfter(10 seconds) {
      val r = orclave {
        val b = true
        if(b) 1 else stop
      }
      r.toList should be(List(1))
    }
    failAfter(10 seconds) {
      val b = false
      val r = orclave {
        if(b) 1 else stop
      }
      r.toList should be(List())
    }
  }
  
  it should "support if statements in functions" in {
    failAfter(10 seconds) {
      val r = orclave {
        def f(x: Int): Int = if(x > 0) 1 else 2
        def g(x: Int): Int = if(x > 0) 3 else stop
        f(1) |||
        f(0) |||
        g(1) |||
        g(0)
      }
      val s = r.toList 
      s.size should be(3)
      s should contain(1)
      s should contain(2)
      s should contain(3)
    }
  }
}
