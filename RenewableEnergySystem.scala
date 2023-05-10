import java.io.{File, FileWriter, PrintWriter}
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import com.github.tototoshi.csv._
import java.io.File
import scala.io.Source

import scala.io.{Source, StdIn}
//Zisong Chen, Jun Pan, Yixiang Wang
object RenewableEnergySystem extends App {
  //  Define output file path and power plant storage capacity
  val outputPath = "D:\\作业\\论文\\dmm2\\output2.csv"
  val storageCapacity = 50000.0
  //Energy class and its subclasses
  abstract class Energy(val energyType: String) {
    def readEnergyProduction(maxValue: Double): Double = {
      print(s"Enter the $energyType production: ")
      val value = StdIn.readDouble()

      if (value >= 0 && value <= maxValue) {
        value
      } else {
        println(s"Invalid value. Please enter a value between 0 and $maxValue.")
        readEnergyProduction(maxValue)
      }
    }
  }

  class SolarPower extends Energy("SolarPower")

  class HydroPower extends Energy("HydroPower")

  class WindPower extends Energy("WindPower")

  // High-order function to read energy with a specific Energy instance
  def readEnergyWith(energyInstance: Energy, maxValue: Double, condition: Double => Boolean): Double = {
    if (condition(maxValue)) {
      energyInstance.readEnergyProduction(maxValue)
    } else {
      0.0
    }
  }

  //Date format
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy/M/d H:mm")
  // Read the total power from the CSV file
  def readTotalElectricity(): Double = {
    val source = Source.fromFile(outputPath)
    val lines = source.getLines().toList
    source.close()

    if (lines.isEmpty || lines.length == 1) {
      return 0.0
    }
    // return 0 if there is no data except the first line
    val lastLine = lines.last
    val values = lastLine.split(',')
    // Get individual data
    if (values.length < 4 || !values(0).matches("\\d{4}/\\d{1,2}/\\d{1,2}\\s\\d{1,2}:\\d{1,2}")) {
      throw new RuntimeException("Output file has incorrect format.")
    }
    // Check the file format
    if (!values(1).matches("SolarPower|HydroPower|WindPower|Consume") || !values(2).matches("\\d+(\\.\\d+)?") || !values(3).matches("\\d+(\\.\\d+)?")) {
      throw new RuntimeException("Output file has incorrect format.")
    }

    values(3).toDouble
  }

  // Write data to CSV file
  def writeToOutput(data: List[String]): Unit = {
    val writer = CSVWriter.open(new FileWriter(outputPath, true))
    writer.writeRow(data)
    writer.close()
  }

  // Extract the date and time from the last line
  def getLastLineInOutput(): Option[String] = {
    val file = new File(outputPath)
    if (file.exists()) {
      val lines = Source.fromFile(file).getLines().toList
      val lastLineWithData = lines.reverse.find(line => !line.startsWith("Date") && !line.trim.isEmpty)
      lastLineWithData
    } else {
      None
    }
  }



  // Extract the date and time from the last line
  def extractDateTimeFromLastLine(lastLine: Option[String]): Option[LocalDateTime] = {
    lastLine.flatMap { line =>
      val columns = line.split(',')
      if (columns.length >= 1) {
        try {
          Some(LocalDateTime.parse(columns(0), dateFormatter))
        } catch {
          case _: DateTimeParseException => None
        }
      } else {
        None
      }
    }
  }
  val lowProductionOutputPath = "D:\\作业\\论文\\dmm2\\low_production_output.csv"
  //transfer data in file into double
  def readDoubleWithCheck(check: Double => Boolean, errorMsg: String): Double = {
    val value = StdIn.readDouble()
    if (check(value)) {
      value
    } else {
      println(errorMsg)
      readDoubleWithCheck(check, errorMsg)
    }
  }

  def checkLowEnergyProduction(): Unit = {
    val file = new File(outputPath)
    if (!file.exists()) {
      println("Output file not found.")
    } else {
      val source = Source.fromFile(outputPath)
      val lines = source.getLines().drop(1).toList // Skip the header line
      source.close()

      // function to write low production data to the CSV file
      def writeLowProductionDataToCsv(lowProductionData: List[String]): Unit = {
        val writer = CSVWriter.open(new FileWriter(lowProductionOutputPath))
        writer.writeRow(List("Date", "Low Production"))
        lowProductionData.foreach(line => writer.writeRow(line.split(": ")))
        writer.close()
      }

      val lowProductionEntries = lines.flatMap { line =>
        val values = line.split(',')

        if (values.length < 4) {
          None
        } else {
          val date = values(0)
          val energyType = values(1)
          val energyProduction = values(2).toDouble
          //if produce lower than 40, we consider it as low produce
          if (energyProduction < 40 && (energyType == "SolarPower" || energyType == "WindPower" || energyType == "HydroPower")) {
            Some(s"Low production on $date for $energyType: $energyProduction")
          } else {
            None
          }
        }
      }

      if (lowProductionEntries.nonEmpty) {
        println("Low energy production detected:")
        lowProductionEntries.foreach(println)

        // Write low production data to the CSV file
        writeLowProductionDataToCsv(lowProductionEntries)

      } else {
        println("No low energy production detected.")
      }
    }
  }



  mainLoop()

  def mainLoop(): Unit = {


    while (true) {
      val totalElectricity = readTotalElectricity()
      val percentage = totalElectricity / storageCapacity
      println(s"Current energy: $totalElectricity / $storageCapacity")

      println("1. Add data\n(PS:never do adjustments when you open output.csv,keep it closed when you select 1)")
      println("2. Equipment inspection")
      println("3. Exit")
      print("Choose an option: ")
      val option = StdIn.readInt()

      option match {
        case 1 =>
          val lastDateTime = extractDateTimeFromLastLine(getLastLineInOutput)

          def readDateTime(): LocalDateTime = {
            lastDateTime match {
              case Some(dt) => println(s"Last recorded date and time: ${dt.format(dateFormatter)}")//get date information in file, if no date information in file show any time
              case None => println("You can choose any time.")
            }

            try {
              print("Enter the current date and time (yyyy/M/d H:mm): ")
              val inputDateTime = LocalDateTime.parse(StdIn.readLine(), dateFormatter)

              if (lastDateTime.isEmpty || inputDateTime.isAfter(lastDateTime.get)) {
                inputDateTime
              } else {
                println("The entered date and time must be later than the last recorded date and time. Please try again.")//handle error if date information is earlier than last time
                readDateTime()
              }

            } catch {
              case e: DateTimeParseException =>
                println("Invalid date format. Please enter the date and time in the correct format (yyyy/M/d H:mm).")//handle error if date format is wrong
                readDateTime()
            }
          }

          val dateTime = readDateTime()

          if (percentage < 0.1) {
            println("Warning: Energy below 10%. Ignoring consumption.")
          } else if (percentage >= 0.9) {
            println("Warning: Energy above 90%. Ignoring production.")
          }
          //prevent error produce message
          val maxProduction = storageCapacity - totalElectricity
          val maxConsumption = totalElectricity
          val solarPowerInstance = new SolarPower
          val hydroPowerInstance = new HydroPower
          val windPowerInstance = new WindPower

          val solarPower = readEnergyWith(solarPowerInstance, maxProduction, _ => percentage < 0.9)
          val hydroPower = readEnergyWith(hydroPowerInstance, maxProduction, _ => percentage < 0.9)
          val windPower = readEnergyWith(windPowerInstance, maxProduction, _ => percentage < 0.9)
          val consumption = readEnergyWith(new Energy("Consumption") {}, maxConsumption, _ => percentage > 0.1)

          val newTotalElectricitySolar = totalElectricity + solarPower
          val newTotalElectricityHydro = newTotalElectricitySolar + hydroPower
          val newTotalElectricityWind = newTotalElectricityHydro + windPower
          val newTotalElectricityConsume = newTotalElectricityWind - consumption
          //calculation of new data and store them in ouput file
          writeToOutput(List(dateTime.format(dateFormatter), "SolarPower", solarPower.toString, newTotalElectricitySolar.toString))
          writeToOutput(List(dateTime.format(dateFormatter), "HydroPower", hydroPower.toString, newTotalElectricityHydro.toString))
          writeToOutput(List(dateTime.format(dateFormatter), "WindPower", windPower.toString, newTotalElectricityWind.toString))
          writeToOutput(List(dateTime.format(dateFormatter), "Consume", consumption.toString, newTotalElectricityConsume.toString))

        case 2 =>
          checkLowEnergyProduction()
        case 3 =>
          println("exiting the program.")

          System.exit(0)

        case _ =>
          println("Invalid option. Please choose 1 or 2 or3.")
      }
    }
  }
// input examine, make sure that data is correct.
  def readAndValidatePower(powerType: String, totalElectricity: Double): Double = {
    print(s"Enter the $powerType: ")
    val power = StdIn.readDouble()

    if (power < 0) {
      println(s"Error: $powerType cannot be negative. Please enter a valid number.")
      readAndValidatePower(powerType, totalElectricity)
    } else if (power + totalElectricity > storageCapacity) {
      println(s"Error: $powerType is too large. The total electricity cannot exceed the storage capacity. Please enter a valid number.")
      readAndValidatePower(powerType, totalElectricity)
    } else {
      power
    }
  }
  // Initialize output.csv with headers if it doesn't exist
  if (!new File(outputPath).exists()) {
    try {
      val outputWriter = new PrintWriter(new File(outputPath))
      outputWriter.write("Date,Type,Electricity,TotalElectricity\n")
      outputWriter.close()
    } catch {
      case e: Exception =>
        println(s"Error: Unable to create output file. Make sure the file path is correct and not in use by another application. Error details: ${e.getMessage}")
    }
  }


}


