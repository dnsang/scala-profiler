package education.x.commons

import scala.concurrent.{ExecutionContext, Future}

class Profiler {

  implicit val ec = ExecutionContext.global


  def apply[A](f: => A): A = {
    println("apply  sync")
    f
  }


  def apply[A](f: => Future[A]): Future[A] = {
    println("apply  async")
    f.onComplete(_ => {
      println(s"async completed")
    })
    f
  }
}


object Profiler {

  implicit val ex = ExecutionContext.global


  def apply(key: String): Profiler = {
    new Profiler()
  }

}
