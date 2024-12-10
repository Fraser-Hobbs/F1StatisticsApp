import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Handles data loading, parsing, and validation for the F1 Statistics App.
 *
 * This handler is responsible for:
 * - Loading data from the file system.
 * - Parsing the raw data into a structured `F1Data` format.
 * - Validating the data and identifying malformed entries.
 *
 * Key Types:
 * - `F1Data`: A map of years to driver statistics.
 * - `F1Driver`: A tuple representing a driver's name, total points, and wins.
 */
object DataHandler {

  /**
   * Represents the overall dataset for F1 statistics.
   * Example:
   * {{{
   * val f1Data: F1Data = Map(
   *   2023 -> List(("Max Verstappen", 575.0, 19), ("Sergio Perez", 285.0, 2))
   * )
   * }}}
   */
  type F1Data = Map[Int, List[F1Driver]]

  /**
   * Represents an individual driver's performance in a season.
   *
   * Example:
   * {{{
   * val driver: F1Driver = ("Max Verstappen", 575.0, 19)
   * }}}
   *
   * Structure:
   * - `String`: Driver's name
   * - `Float`: Driver's total points
   * - `Int`: Number of wins in the season
   */
  type F1Driver = (String, Float, Int)

  /**
   * Loads F1 dataset from a file and parses it into the F1Data structure.
   *
   * @param filePath Path to the dataset file.
   * @return Parsed F1Data map where the key is the year and the value is a list of drivers with their stats.
   */
  def loadDataFromFile(filePath: String): F1Data = {
    if (!fileExists(filePath)) {
      throw new IllegalArgumentException(s"File not found at: $filePath")
    }

    Try(Source.fromFile(filePath)) match {
      case Success(source) =>
        val data = parseDataset(source.getLines().toList)
        source.close()
        println("Data loaded successfully!")
        data
      case Failure(exception) =>
        throw new RuntimeException(s"Error reading file: ${exception.getMessage}")
    }
  }

  /**
   * Checks if a file exists at the given path.
   *
   * @param filePath Path to the file.
   * @return True if the file exists, False otherwise.
   */
  private def fileExists(filePath: String): Boolean = Files.exists(Paths.get(filePath))

  /**
   * Parses the dataset into F1Data.
   *
   * @param lines List of lines from the dataset file.
   * @return Parsed F1Data map.
   */
  private def parseDataset(lines: List[String]): F1Data = {
    var malformedEntries: List[String] = List()

    val parsedData = lines.foldLeft(Map.empty[Int, List[F1Driver]]) { (map, line) =>
      val parts = line.split(",", 2) // Split into year and driver details

      // Validate the structure of the line
      if (parts.length == 2 && Try(parts(0).toInt).isSuccess) {
        val year = parts(0).toInt // Parse the year
        val drivers = parts(1).split(",").toList.map { entry =>
          val parsedDriver = parseDriver(entry)
          if (parsedDriver._1 == "Unknown") {
            malformedEntries = malformedEntries :+ s"Year $year: $entry" // Track malformed entries
          }
          parsedDriver
        }
        map + (year -> drivers)
      } else {
        malformedEntries = malformedEntries :+ s"Invalid line structure: $line" // Log invalid lines
        map
      }
    }

    if (malformedEntries.nonEmpty) {
      println("\nMalformed entries detected:")
      malformedEntries.foreach(entry => println(s"- $entry"))
    }

    parsedData
  }

  /**
   * Parses a single driver entry into an F1Driver tuple.
   *
   * @param entry Raw string entry for a driver.
   * @return Parsed F1Driver tuple (Name, Points, Wins).
   */
  private def parseDriver(entry: String): F1Driver = {
    val driverPattern = """^([\w\s]+):\s(\d+(\.\d+)?)\s+(\d+)$""".r
    entry.trim match {
      case driverPattern(name, pointsStr, _, winsStr) =>
        (name, pointsStr.toFloatOption.getOrElse(0.0f), winsStr.toIntOption.getOrElse(0))
      case _ =>
        println(s"Malformed driver entry: $entry") // Log malformed entries
        ("Unknown", 0.0f, 0)
    }
  }
}