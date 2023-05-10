import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.Source
import scala.io.StdIn.readLine
import java.time.format.DateTimeParseException
//Zisong Chen, Jun Pan, Yixiang Wang
object dataView {
  case class DataPoint(dateTime: LocalDateTime, hydro: Double, wind: Double, solar: Double, consumption: Double, storage: Double)

  def main(args: Array[String]): Unit = {
    // Set the maximum storage capacity
    val maxStorageCapacity = 50000
    //Set Boolean variables for program continuation loop
    var continue = true
    while (continue) {
      // Read the data from the CSV file
      val data = readData("D:\\作业\\论文\\dmm2\\output2.csv")
      // Sort the data by date time
      val sortedData = data.sortBy(_.dateTime)

      val firstDataPoint = sortedData.head
      val latestDataPoint = sortedData.last
      // print the data recorded at the beginning of the csv file
      displayCurrentData(firstDataPoint, "Start of the record", maxStorageCapacity)
      // print the last recorded data of the csv file
      displayCurrentData(latestDataPoint, "End of the record", maxStorageCapacity)

      // Read the start and end date and time of the data analysis
      var BeDay: LocalDateTime = null
      var FiDay: LocalDateTime = null
      val formatter = DateTimeFormatter.ofPattern("yyyy/M/d H:mm")

      do {
        try {
          println("Enter start time (yyyy/M/d H:mm):")
          val startTime = readLine()
          BeDay = LocalDateTime.parse(startTime, formatter)

          println("Enter end time (yyyy/M/d H:mm):")
          val endTime = readLine()
          FiDay = LocalDateTime.parse(endTime, formatter)

          if (BeDay.isAfter(FiDay)) {
            println("Start time must be before end time. Please try again.")
            BeDay = null
            FiDay = null
          }
        } catch {
          case _: DateTimeParseException =>
            println("Invalid date format. Please use the format: yyyy/M/d H:mm")
        }
      } while (BeDay == null || FiDay == null)
      // filtered data date and time
      val filteredData = filterData(data, BeDay, FiDay)
      // calculate the total energy production from hydro, wind and solar
      val (totalHydro, totalWind, totalSolar) = calculateTotalEnergy(filteredData)
      // Calculate the percentage of total energy produced by hydro, wind and solar resources
      val (hype, wipe, sope) = calculateEnergyPercentages(totalHydro, totalWind, totalSolar)
      // find the times when energy production is below 400kwh
      val lowProductionTimes = findLowProductionTimes(filteredData, 400)
      // find the minimum and maximum storage levels for the specified time period
      val (minStorage, maxStorage) = findMinMaxStorage(filteredData)
      // find the energy consumption statistics for the specified time period
      val (totalConsumption, productionConsumptionRatio, highestConsumption, lowestConsumption) = findConsumptionStats(filteredData)



      printPieChart(totalHydro, totalWind, totalSolar)
      println(s"Percentage of total energy produced for the time period: Hydro: $hype%, Wind: $wipe%, Solar: $sope%")
      println()
      println(s"Low generation times (below 400): $lowProductionTimes")
      println(s"Lowest storage: ${minStorage.dateTime} (${minStorage.storage})")
      println(s"Highest storage: ${maxStorage.dateTime} (${maxStorage.storage})")
      println()
      println(s"Total energy consumption for the time period: $totalConsumption")
      println(s"Ratio of total energy production to total energy consumption: $productionConsumptionRatio")
      println()
      println(s"Time of highest consumption: ${highestConsumption.dateTime} (${highestConsumption.consumption})")
      println(s"Time of lowest consumption: ${lowestConsumption.dateTime} (${lowestConsumption.consumption})")
      println("----------------------------")
      println("Do you want to analyze more data? (Yes/No)")
      continue = readLine().toLowerCase == "yes"
    }
  }

  def readData(filename: String): List[DataPoint] = {
    val formatter = DateTimeFormatter.ofPattern("yyyy/M/d H:mm")
    val lines = Source.fromFile(filename).getLines().drop(1).toSeq.view  // Use View

    val parsedLines = lines
      .map { line =>
        val Array(dateTimeStr, energyType, electricity, totalElectricity) = line.split(",").map(_.trim)
        val dateTime = LocalDateTime.parse(dateTimeStr, formatter)
        (dateTime, energyType, electricity.toDouble, totalElectricity.toDouble)
      }
      .groupBy(_._1)

    parsedLines.map {
      case (dateTime, endata) =>
        val hydro = endata.find(_._2 == "HydroPower").map(_._3).getOrElse(0.0)
        val wind = endata.find(_._2 == "WindPower").map(_._3).getOrElse(0.0)
        val solar = endata.find(_._2 == "SolarPower").map(_._3).getOrElse(0.0)
        val consumption = endata.find(_._2 == "Consume").map(_._3).getOrElse(0.0)
        val storage = endata.find(_._2 == "Consume").map(_._4).getOrElse(0.0)

        DataPoint(dateTime, hydro, wind, solar, consumption, storage)
    }.toList
  }



  def filterData(data: List[DataPoint], BeDay: LocalDateTime, FiDay: LocalDateTime): List[DataPoint] = {
    data.filter(d => !d.dateTime.isBefore(BeDay) && !d.dateTime.isAfter(FiDay))
  }

  def calculateTotalEnergy(data: List[DataPoint]): (Double, Double, Double) = {
    data.view.foldLeft((0.0, 0.0, 0.0)) { case ((hydro, wind, solar), point) =>//Data is converted to List.view
      (hydro + point.hydro, wind + point.wind, solar + point.solar)
    }
  }


  def calculateEnergyPercentages(hydro: Double, wind: Double, solar: Double): (Double, Double, Double) = {
    val total = hydro + wind + solar
    (hydro / total * 100, wind / total * 100, solar / total * 100)
  }

  def findLowProductionTimes(data: List[DataPoint], threshold: Double): List[String] = {
    val filteredData = data.view.filter(point => point.hydro < threshold || point.wind < threshold)//Data is converted to List.view
    val sortedData = filteredData.force.sortBy(_.dateTime)
    sortedData.map(point => point.dateTime.format(DateTimeFormatter.ofPattern("yyyy/M/d H:mm"))).toList
  }


  def findMinMaxStorage(data: List[DataPoint]): (DataPoint, DataPoint) = {
    data.view.foldLeft((data.head, data.head)) { case ((min, max), point) =>//Data is converted to List.view
      val newMin = if (point.storage < min.storage) point else min
      val newMax = if (point.storage > max.storage) point else max
      (newMin, newMax)
    }
  }

  def findConsumptionStats(data: List[DataPoint]): (Double, Double, DataPoint, DataPoint) = {
    val (highestConsumption, lowestConsumption, totalConsumption) = data.view.foldLeft((data.head, data.head, 0.0)) {
      case ((highest, lowest, total), point) =>
        (
          if (point.consumption > highest.consumption) point else highest,
          if (point.consumption < lowest.consumption) point else lowest,
          total + point.consumption
        )
    }

    val totalEnergyProduction = data.map(point => point.hydro + point.wind + point.solar).sum
    val productionConsumptionRatio = totalEnergyProduction / totalConsumption

    (totalConsumption, productionConsumptionRatio, highestConsumption, lowestConsumption)
  }

  def displayCurrentData(dataPoint: DataPoint, title: String, maxStorageCapacity: Double): Unit = {
    println(s"$title (${dataPoint.dateTime}):")
    printPieChart(dataPoint.hydro,dataPoint.wind,dataPoint.solar)
    println(s"Consumption: ${dataPoint.consumption}")
    printFanChart(dataPoint.storage, maxStorageCapacity)
    println("----------------------------")
  }

  def printFanChart(currentStorage: Double, maxStorageCapacity: Double): Unit = {
    val totalS = 20
    val storageRatio = currentStorage / maxStorageCapacity
    val usedSegments = (storageRatio * totalS).round.toInt
    val unusedSegments = totalS - usedSegments

    val usedRepresentation = "▒" * usedSegments
    val unusedRepresentation = "░" * unusedSegments

    println(s"Storage: $usedRepresentation$unusedRepresentation ${storageRatio * 100} %     currentStorage: $currentStorage/50000")
  }



  def printPieChart(a: Double, b: Double, c: Double): Unit = {
    val (hype, wipe, sope) = calculateEnergyPercentages(a, b, c)
    val totalS = 20
    val hydroSegments = (hype / 100 * totalS).round.toInt
    val windSegments = (wipe / 100 * totalS).round.toInt
    val solarSegments = totalS - (hydroSegments + windSegments)

    val hydata = "█" * hydroSegments
    val widata = "█" * windSegments
    val sodata = "█" * solarSegments

    println(s"Hydro:  $hydata $hype%          $a")
    println(s"Wind:   $widata $wipe%          $b")
    println(s"Solar:  $sodata $sope%          $c")
  }


}