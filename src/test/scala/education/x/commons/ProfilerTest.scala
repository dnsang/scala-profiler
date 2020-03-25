package education.x.commons

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Future
import org.scalatest.concurrent.ScalaFutures._

class ProfilerTest extends AnyFunSuite {

  implicit val ec = scala.concurrent.ExecutionContext.global
  test("Test Profiler") {

    val sumValue = Profiler("ProfilerTest.sum") {
      sum(100)
    }
    val fSumValue = Profiler("ProfilerTest::asyncSum") {
      aSum(100)
    }


    whenReady(fSumValue) {
      result => assert(result == sumValue)
    }

    Profiler.disable()


    Profiler("ProfilerTest.sum") {
      sum(100)
    }

    Profiler.enable()

    Profiler("ProfilerTest.sum") {
      sum(1200)
    }
    Profiler("ProfilerTest.sum") {
      sum(1003)
    }
    Profiler("ProfilerTest.sum") {
      sum(1040)
    }
    Profiler("ProfilerTest.sum") {
      sum(5100)
    }
    Profiler("ProfilerTest::asyncSum") {
      aSum(100)
    }
    Profiler("ProfilerTest::asyncSum") {
      aSum(100)
    }
    Profiler("ProfilerTest::asyncSum") {
      aSum(200)
    }
    Profiler("ProfilerTest::asyncSum") {
      aSum(300)
    }
    Profiler("ProfilerTest::asyncSum") {
      aSum(400)
    }




    println(Profiler.report())
    println(Profiler.getHistory("ProfilerTest.sum"))
    println(Profiler.getHistory())
    println(Profiler.reportAsHtml())

  }

  def sum(n: Int): Int = {
    require(n >= 0)
    if (n == 0) 0 else n + sum(n - 1)
  }

  def aSum(n: Int): Future[Int] = {
    Future {
      sum(n)
    }
  }


}
