package education.x.commons

import java.util.concurrent.atomic.{AtomicLong, LongAdder}


trait MeasureService {

  type ProfileId = String

  def startMeasure(funcName: String): ProfileId

  def stopMeasure(profileId: ProfileId)

  def reportAsText(): String

  def reportAsHtml(): String

  def disable(funcName: String)

  def enable(funcName: String)

  def disableAll()

  def enableAll()

}


case class Record(atTime: Long, executionTime: Long)

class MeasureValue(funcName: String) {
  val maxHistoryRecord: Int = System.getProperty("ProfilerMaxHistory", "1000").toInt
  val totalTime = new LongAdder()
  val totalPending = new LongAdder()
  val totalHit = new LongAdder()
  val historyRecords = new FixedQueue[Record](maxHistoryRecord)

  def startMeasure(): Unit = {
    totalPending.increment()
    totalHit.increment()
  }

  def stopMeasure(executionTime: Long): Unit = {
    totalPending.decrement()
    totalTime.add(executionTime)
    historyRecords += Record(System.currentTimeMillis(),executionTime)
  }
}

class CumulativeMeasureService extends MeasureService {

  override def startMeasure(funcName: String): ProfileId = {

  }

  override def stopMeasure(profileId: ProfileId): Unit = ???

  override def reportAsText(): String = ???

  override def reportAsHtml(): String = ???

  override def disable(funcName: String): Unit = ???

  override def enable(funcName: String): Unit = ???

  override def disableAll(): Unit = ???

  override def enableAll(): Unit = ???
}