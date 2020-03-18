package education.x.commons

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Future

class ProfilerTest extends AnyFunSuite {

  implicit val ec = scala.concurrent.ExecutionContext.global
  test("Test Profiler") {

    Profiler("ProfilerTest::testSyncFunc")(
      () => {
        println("execution")
      }
    )
    Profiler("ProfilerTest::xtestSyncFunc")(
      () => {
        println("execution")
      }
    )

    val fValue = Profiler("ProfilerTest::testAsyncFunc") {
      asyncFn(3)
    }

    fValue.onComplete(f => {
      println(s"Async Value = ${f.get}")
    })

    Profiler.disable()


    Profiler("ProfilerTest::testSyncFunc")(
      () => {
        println("execution")
      }
    )

    Profiler.enable()

    Profiler("ProfilerTest::testSyncFunc")(
      () => {
        println("execution")
      }
    )

    Thread.sleep(5000)


    println(Profiler.report())
    println(Profiler.getHistory("ProfilerTest::testSyncFunc"))
    println(Profiler.getHistory())

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
