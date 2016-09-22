package orc.scala.test

import org.scalatest._
import org.scalatest.concurrent.TimeLimits._
import org.scalatest.time.SpanSugar._

import orc.scala._
import Orc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class SimpleRunTests extends FlatSpec with Matchers {
  implicit val ctx = OrcExecutionContext(ExecutionContext.global)

  "OrcScal runtime" should "execute constant expressions" in {
    val r = orc { 1 }
    r.toList should be(List(1))
  }

  it should "execute parallel constants" in {
    val r = orc { 1 ||| 2 }
    r.toList should contain theSameElementsAs (List(1, 2))
  }

  it should "execute stop" in {
    val r = orc { stop }
    r.toList.size should be(0)
  }

  it should "execute side-effect at the correct time" in {
    var x = 0
    val o = orcRaw { x = 42 }
    x should be(0)
    val r = orc(o)
    r.toSet should be(Set(()))
    x should be(42)
  }

  it should "execute graft" in {
    failAfter(10 seconds) {
      val r = orc {
        val x = (42).graft
        x.body |||
          x.future
      }
      r.toList should be(List(42))
    }
    failAfter(10 seconds) {
      val r = orc {
        val x = (stop).graft
        x.body |||
          x.future
      }
      r.toList should be(List())
    }
  }

  it should "execute otherwise" in {
    failAfter(10 seconds) {
      val r = orc {
        stop otherwise 42
      }
      r.toList should be(List(42))
    }
    failAfter(10 seconds) {
      val r = orc {
        5 otherwise 42
      }
      r.toList should be(List(5))
    }
  }

  it should "execute trim" in {
    failAfter(10 seconds) {
      val r = orc {
        trim { 1 ||| 2 }
      }
      val s = r.toList
      s.size should be(1)
      s should contain oneOf (1, 2)
    }
    failAfter(10 seconds) {
      val r = orc {
        trim { scalaExpr({ Thread.sleep(100); 1 }) ||| 2 }
      }
      val s = r.toList
      s.size should be(1)
      s should contain(2)
    }
    failAfter(10 seconds) {
      val r = orc {
        trim { 1 ||| scalaExpr({ Thread.sleep(100); 2 }) }
      }
      val s = r.toList
      s.size should be(1)
      s should contain(1)
    }
  }

  it should "execute branch" in {
    failAfter(10 seconds) {
      val r = orc {
        for (x <- 3) yield {
          x + 1
        }
      }
      r.toList should contain theSameElementsAs List(4)
    }
    failAfter(10 seconds) {
      val r = orc {
        for (x <- 1 ||| 2) yield {
          x + 1
        }
      }
      r.toList should contain theSameElementsAs List(2, 3)
    }
  }

  it should "execute branch on future" in {
    failAfter(10 seconds) {
      val r = orc {
        val x = scalaExpr(Thread.sleep(100)).graft
        x.body ||| 1 ||| {
          for (_ <- variable(x.future)) yield {
            4
          }
        }
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orc {
        val x = scalaExpr(Thread.sleep(100)).graft
        x.body ||| {
          for (_ <- variable(x.future)) yield {
            4
          }
        } ||| 1
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orc {
        val x = scalaExpr(Thread.sleep(100)).graft
        (
          for (_ <- variable(x.future)) yield {
            4
          }) ||| 1 ||| x.body
      }
      r.toList should be(List(1, 4))
    }
  }

  it should "execute branch with sleep" in {
    failAfter(10 seconds) {
      val r = orc {
        1 ||| {
          for (_ <- scalaExpr(Thread.sleep(100))) yield {
            4
          }
        }
      }
      r.toList should be(List(1, 4))
    }
    failAfter(10 seconds) {
      val r = orc {
        {
          for (_ <- scalaExpr(Thread.sleep(100))) yield {
            4
          }
        } ||| 1
      }
      r.toList should be(List(1, 4))
    }
  }

  it should "execute otherwise with sleep" in {
    failAfter(10 seconds) {
      val r = orc {
        1 ||| {
          (for (_ <- scalaExpr(Thread.sleep(100))) yield {
            stop
          }) otherwise 3
        }
      }
      r.toList should be(List(1, 3))
    }
    failAfter(10 seconds) {
      val r = orc {
        {
          (for (_ <- scalaExpr(Thread.sleep(100))) yield {
            stop
          }) otherwise 3
        } ||| 1
      }
      r.toList should be(List(1, 3))
    }
  }

  def badSleep(n: Long): Orc[Unit] = scalaExpr(Thread.sleep(n))

  it should "execute parallel with sleep" in {
    failAfter(10 seconds) {
      val r = orc {
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
      val r = orc {
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
      val r = orc {
        trim {
          1 ||| {
            (for (_ <- badSleep(100)) yield {
              x = 1
            })
          }
        }
      }
      x should be(0)
      r.toList should be(List(1))
      x should be(0)
    }
    failAfter(10 seconds) {
      var x = 0
      val r = orc {
        trim {
          {
            (for (_ <- badSleep(100)) yield {
              x = 1
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
      val r = orc {
        trim {
          {
            (for (_ <- badSleep(200)) yield {
              x = 1
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
}