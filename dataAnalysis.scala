//Zisong Chen, Jun Pan, Yixiang Wang
import scala.io.Source
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.WeekFields
import java.util.Locale

case class DataPoint(date: LocalDateTime, dataType: String, electricity: Double, totalElectricity: Double)

object dataAnalysis extends App {
  def readData(filename: String): List[DataPoint] = {
    Source.fromFile(filename).getLines().drop(2).map { line =>
      val columns = line.split(",").map(_.trim)
      val date = LocalDateTime.parse(columns(0), DateTimeFormatter.ofPattern("yyyy/M/d H:mm"))
      val dataType = columns(1)
      val electricity = columns(2).toDouble
      val totalElectricity = columns(3).toDouble
      DataPoint(date, dataType, electricity, totalElectricity)
    }.toList
  }

  def analyzeHourlyData(filename: String): (Map[String, (Double, Double, Double, Double, Double)], (Double, Double, Double, Double, Double)) = {
    val data = readData(filename)

    val groupedData = data.groupBy(dp => dp.dataType)

    val dataTypeStats = groupedData.map {
      case (dataType, dataPoints) =>
        val values = dataPoints.map(_.electricity).toList
        val average = values.sum / values.length

        val sortedValues = values.sorted
        val median = if (values.length % 2 == 0) {
          (sortedValues(values.length / 2 - 1) + sortedValues(values.length / 2)) / 2
        } else {
          sortedValues(values.length / 2)
        }

        val mode = values.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

        val range = sortedValues.last - sortedValues.head

        val midRange = (sortedValues.head + sortedValues.last) / 2

        dataType -> (average, median, mode, range, midRange)
    }

    val totalElectricityList = data.map(_.totalElectricity).toList
    val totalElectricityAverage = totalElectricityList.sum / totalElectricityList.length

    val sortedTotalElectricity = data.sortBy(_.totalElectricity)
    val totalElectricityMedian = if (data.length % 2 == 0) {
      (sortedTotalElectricity(data.length / 2 - 1).totalElectricity + sortedTotalElectricity(data.length / 2).totalElectricity) / 2
    } else {
      sortedTotalElectricity(data.length / 2).totalElectricity
    }

    val totalElectricityMode = totalElectricityList.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

    val totalElectricityRange = sortedTotalElectricity.last.totalElectricity - sortedTotalElectricity.head.totalElectricity

    val totalElectricityMidRange = (sortedTotalElectricity.head.totalElectricity + sortedTotalElectricity.last.totalElectricity) / 2

    (dataTypeStats, (totalElectricityAverage, totalElectricityMedian, totalElectricityMode, totalElectricityRange, totalElectricityMidRange))
  }

  def analyzeData(filename: String, filterFunction: DataPoint => Boolean, groupingFunction: DataPoint => Any): (Map[String, (Double, Double, Double, Double, Double)], (Double, Double, Double, Double, Double)) = {
    val data = readData(filename)
    val filteredData = data.filter(filterFunction)
    val groupedData = filteredData.groupBy(dp => (dp.dataType, groupingFunction(dp)))

    val dataTypeStats = groupedData.map {
      case ((dataType, _), dataPoints) =>
        val weekSum = dataPoints.map(_.electricity).sum
        (dataType, weekSum)
    }.groupBy(_._1).map {
      case (dataType, dataSums) =>
        val values = dataSums.map(_._2).toList
        val average = values.sum / values.length

        val sortedValues = values.sorted
        val median = if (values.length % 2 == 0) {
          (sortedValues(values.length / 2 - 1) + sortedValues(values.length / 2)) / 2
        } else {
          sortedValues(values.length / 2)
        }

        val mode = values.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

        val range = sortedValues.last - sortedValues.head

        val midRange = (sortedValues.head + sortedValues.last) / 2

        dataType -> (average, median, mode, range, midRange)
    }

    val totalElectricityList = filteredData.map(_.totalElectricity).toList
    val totalElectricityAverage = totalElectricityList.sum / totalElectricityList.length

    val sortedTotalElectricity = filteredData.sortBy(_.totalElectricity)
    val totalElectricityMedian = if (filteredData.length % 2 == 0) {
      (sortedTotalElectricity(filteredData.length / 2 - 1).totalElectricity + sortedTotalElectricity(filteredData.length / 2).totalElectricity) / 2
    } else {
      sortedTotalElectricity(filteredData.length / 2).totalElectricity
    }

    val totalElectricityMode = totalElectricityList.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

    val totalElectricityRange = sortedTotalElectricity.last.totalElectricity - sortedTotalElectricity.head.totalElectricity

    val totalElectricityMidRange = (sortedTotalElectricity.head.totalElectricity + sortedTotalElectricity.last.totalElectricity) / 2

    (dataTypeStats, (totalElectricityAverage, totalElectricityMedian, totalElectricityMode, totalElectricityRange, totalElectricityMidRange))
  }


  def weeksOfMonth(year: Int, month: Int): List[Int] = {
    val firstDayOfMonth = LocalDateTime.of(year, month, 1, 0, 0)
    val lastDayOfMonth = firstDayOfMonth.plusMonths(1).minusDays(1)
    val weekField = WeekFields.of(Locale.getDefault()).weekOfWeekBasedYear()

    (firstDayOfMonth.get(weekField) to lastDayOfMonth.get(weekField)).toList
  }

  def printDataPoint(dp: DataPoint): Unit = {
    println(s"Date: ${dp.date.format(DateTimeFormatter.ofPattern("yyyy/M/d H:mm"))}")
    println(s"Data Type: ${dp.dataType}")
    println(s"Electricity: ${dp.electricity}")
    println(s"Total Electricity: ${dp.totalElectricity}")
  }

  def printResults(dataTypeStats: Map[String, (Double, Double, Double, Double, Double)], totalElectricityStats: (Double, Double, Double, Double, Double)): Unit = {
    dataTypeStats.foreach {
      case (dataType, (average, median, mode, range, midRange)) =>
        println(s"\nData Type: $dataType")
        println(s"Mean (Electricity): $average")
        println(s"Median (Electricity): $median")
        println(s"Mode (Electricity): $mode")
        println(s"Range (Electricity): $range")
        println(s"Midrange (Electricity): $midRange")
    }

    println("\nTotal Electricity")
    println(s"Mean: ${totalElectricityStats._1}")
    println(s"Median: ${totalElectricityStats._2}")
    println(s"Mode: ${totalElectricityStats._3}")
    println(s"Range: ${totalElectricityStats._4}")
    println(s"Midrange: ${totalElectricityStats._5}")
  }


  val filename = "D:\\作业\\论文\\dmm2\\output2.csv"
  val data = readData(filename)
  var userInput = 0
  while (userInput != 6) {
    println("\nPlease select the method of analyzing the data:")
    println("1. Analyze data by hour")
    println("2. Analyze data by week")
    println("3. Analyze data by month")
    println("4. Search for specific dates and types of data")
    println("5. Search for data in a specific date range")
    println("6. Exit")
    print("Enter the number to select the analysis method:")
    userInput = scala.io.StdIn.readInt()

    val weekField = WeekFields.of(Locale.getDefault()).weekOfWeekBasedYear()
    val minDate = data.minBy(_.date).date
    val maxDate = data.maxBy(_.date).date
    val firstWeekOfData = minDate.get(weekField)
    val lastWeekOfData = maxDate.get(weekField)
    val firstMonthOfData = minDate.getMonthValue
    val lastMonthOfData = maxDate.getMonthValue

    val filterFunction: DataPoint => Boolean = userInput match {
      case 1 => _ => true
      case 2 =>
        if (lastWeekOfData - firstWeekOfData < 1) {
          println("Error: The data provided does not span more than one week.")
          _ => false
        } else {
          dp => {
            val currentWeek = dp.date.get(weekField)
            currentWeek > firstWeekOfData && currentWeek < lastWeekOfData
          }
        }
      case 3 =>
        if (lastMonthOfData - firstMonthOfData < 1) {
          println("Error: The data provided does not span more than one month.")
          _ => false
        } else {
          dp => {
            val currentMonth = dp.date.getMonthValue
            currentMonth > firstMonthOfData && currentMonth < lastMonthOfData
          }
        }
      case 4 =>
        print("Please enter the date (format: yyyy/M/d H:mm):")
        val inputDate = LocalDateTime.parse(scala.io.StdIn.readLine(), DateTimeFormatter.ofPattern("yyyy/M/d H:mm"))
        print("Please enter the data type (HydroPower, SolarPower or WindPower):")
        val inputType = scala.io.StdIn.readLine()
        val foundData = data.find(dp => dp.date == inputDate && dp.dataType == inputType)
        foundData match {
          case Some(dp) => printDataPoint(dp)
          case None => println("The data for the specified date and type was not found.")
        }
        _ => false

      case 5 =>
        print("Please enter the start date (format: yyyy/M/d H:mm):")
        val startDate = LocalDateTime.parse(scala.io.StdIn.readLine(), DateTimeFormatter.ofPattern("yyyy/M/d H:mm"))
        print("Please enter the end date (format: yyyy/M/d H:mm):")
        val endDate = LocalDateTime.parse(scala.io.StdIn.readLine(), DateTimeFormatter.ofPattern("yyyy/M/d H:mm"))
        val foundData = data.filter(dp => dp.date.isAfter(startDate) && dp.date.isBefore(endDate))
        if (foundData.isEmpty) {
          println("No data was found for the specified date range.")
        } else {
          foundData.foreach(printDataPoint)
        }
        _ => false

      case 6 => sys.exit()
      case _ =>
        println("Invalid option. Please choose 1 or 2 or 3 or 4 or 5 or 6.")
        _ => false
    }

    val filteredData = data.filter(filterFunction)
    if (userInput == 1) {
      val (dataTypeStats, totalElectricityStats) = analyzeHourlyData(filename)
      printResults(dataTypeStats, totalElectricityStats)
    }else if (filteredData.nonEmpty && userInput != 4 && userInput != 5) {
      val groupingFunction: DataPoint => Any = userInput match {
        case 2 => dp => dp.date.get(weekField)
        case 3 => dp => dp.date.getMonthValue
        case _ => dp => dp.date
      }
      val (dataTypeStats, totalElectricityStats) = analyzeData(filename, filterFunction, groupingFunction)
      printResults(dataTypeStats, totalElectricityStats)
    }


  }
}