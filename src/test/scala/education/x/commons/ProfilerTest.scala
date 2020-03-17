package education.x.commons

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Future
import org.scalatest.concurrent._

class ProfilerTest extends AnyFunSuite {

  //
  //  test("Sync Profiler") {
  //
  //    println("start profiler")
  //    Profiler("test.sync_profiler") {
  //      println("inner method is executed")
  //    }
  //    println("stop profiler")
  //
  //  }


  implicit val ec = scala.concurrent.ExecutionContext.global
  test("ASync Profiler") {
    Profiler("name")(
      () => {
        println("execution")
      }
    )

    val fValue = Profiler("name") {
      asyncFn(3)
    }

    fValue.onComplete(f => {
      println(s"Async Value = ${f.get}")
    })


    Thread.sleep(5000)

  }


  def asyncFn(n: Int): Future[Int] = Future {
    println("Start asyncFunc")
    var sum = 0
    for (i <- 1 to n) {
      print(".")
      sum += i
      Thread.sleep(1000)
    }
    println()
    println("Stop asyncFunc")
    sum

  }

}
