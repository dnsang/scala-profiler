package education.x.commons

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, LongAdder}
import java.util.function.BiConsumer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait MeasureService {

  def startMeasure(funcName: String): Unit

  def stopMeasure(funcName: String, executionTime: Long)

  def reportAsText(): String

  def reportAsHtml(): String

  def disable(funcName: String)

  def enable(funcName: String)

  def disableAll()

  def enableAll()


  def formatTable(table: Seq[Seq[Any]]): String = {
    if (table.isEmpty) ""
    else {
      // Get column widths based on the maximum cell width in each column (+2 for a one character padding on each side)
      val colWidths = table.transpose.map(_.map(cell => if (cell == null) 0 else cell.toString.length).max + 2)
      // Format each row
      val rows = table.map(_.zip(colWidths).map { case (item, size) => (" %-" + (size - 1) + "s").format(item) }
        .mkString("|", "|", "|"))
      // Formatted separator row, used to separate the header and draw table borders
      val separator = colWidths.map("-" * _).mkString("+", "+", "+")
      // Put the table together and return
      (separator +: rows.head +: separator +: rows.tail :+ separator).mkString("\n")
    }
  }

}


case class Record(atTime: Long, executionTime: Long)

class MeasureValue(funcName: String) {
  val maxHistoryRecord: Int = System.getProperty("ProfilerMaxHistory", "1000").toInt
  val totalTime = new LongAdder()
  val numCurrentPending = new LongAdder()
  val totalHit = new LongAdder()
  val historyRecords = new FixedQueue[Record](maxHistoryRecord)

  def startMeasure(): Unit = {
    numCurrentPending.increment()
    totalHit.increment()
  }

  def stopMeasure(executionTime: Long): Unit = {
    numCurrentPending.decrement()
    totalTime.add(executionTime)
    historyRecords += Record(System.currentTimeMillis(), executionTime)
  }
}

class CumulativeMeasureService extends MeasureService {

  val measureValueMap: ConcurrentHashMap[String, MeasureValue] = new ConcurrentHashMap[String, MeasureValue]()

  def getMeasureValue(funcName: String): MeasureValue = {
    if (!measureValueMap.containsKey(funcName))
      measureValueMap.putIfAbsent(funcName, new MeasureValue(funcName))
    measureValueMap.get(funcName)
  }

  override def startMeasure(funcName: String): Unit = {
    getMeasureValue(funcName).startMeasure()
  }

  override def stopMeasure(funcName: String, executionTime: Long): Unit = {
    getMeasureValue(funcName).stopMeasure(executionTime)
  }

  override def reportAsText(): String = {
    val header = Seq("Function", "Total Time (ms)", "Total Hit", "Avg (ms)", "Num Pending")
    val report = ListBuffer[Seq[Any]]()
    report.append(header)
    measureValueMap.forEach(new BiConsumer[String, MeasureValue] {
      override def accept(k: String, v: MeasureValue): Unit = {
        val avg = if (v.totalHit.longValue() == 0) 0 else v.totalTime.longValue() / v.totalHit.longValue()
        report.append(Seq(k, v.totalTime, v.totalHit, avg, v.numCurrentPending.longValue()))
      }
    })

    formatTable(report.toList)
  }

  override def reportAsHtml(): String = ???

  override def disable(funcName: String): Unit = ???

  override def enable(funcName: String): Unit = ???

  override def disableAll(): Unit = ???

  override def enableAll(): Unit = ???

}