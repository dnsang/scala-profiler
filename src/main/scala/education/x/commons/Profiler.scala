package education.x.commons

import scala.concurrent.{ExecutionContext, Future}

trait Profiler {

  def apply[T](f: => T)(implicit ec: ExecutionContext): T

  def apply[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T]
}


object Profiler {

  val measureService: MeasureService = new CumulativeMeasureService

  def apply(funcName: String): Profiler = {

    measureService.startMeasure(funcName)

    new Profiler() {

      override def apply[T](f: => T)(implicit ec: ExecutionContext): T = {
        val t1 = System.currentTimeMillis()
        try {
          f
        } finally {
          measureService.stopMeasure(funcName, System.currentTimeMillis() - t1)
        }
      }

      override def apply[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
        val t1 = System.currentTimeMillis()
        f.onComplete(_ => {
          measureService.stopMeasure(funcName, System.currentTimeMillis() - t1)
        })
        f
      }
    }
  }

  def report(): String = measureService.reportAsText()

  def reportAsHtml(): String = measureService.reportAsHtml()
}
