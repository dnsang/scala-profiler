package education.x.commons

import scala.concurrent.{ExecutionContext, Future}

trait Profiler {

  def apply[T](f: => T)(implicit ec: ExecutionContext = ExecutionContext.global): T

  def apply[T](f: => Future[T])(implicit ec: ExecutionContext = ExecutionContext.global): Future[T]
}


object Profiler {

  val measureService: MeasureService = new CumulativeMeasureService

  def apply(funcName: String): Profiler = {

    val profileId = measureService.startMeasure(funcName)

    new Profiler() {

      override def apply[T](f: => T)(implicit ec: ExecutionContext): T = {

        try {
          f
        } finally {
          measureService.stopMeasure(profileId)
        }
      }

      override def apply[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {

        f.onComplete(_ => {
          measureService.stopMeasure(profileId)
        })
        f
      }
    }
  }
}
