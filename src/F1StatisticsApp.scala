import DataHandler.{F1Data, F1Driver}

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}
import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.Try


object F1StatisticsApp extends App {

  @main
  def main(): Unit = {
    println("Welcome to the F1 Statistics CLI Application")

    try {
      val f1Data = DataHandler.loadDataFromFile("resources/data.txt")
      MenuHandler.menuController(f1Data)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        println("Exiting application...")
      case e: RuntimeException =>
        println(s"Unexpected error: ${e.getMessage}")
        println("Exiting application...")
    }

  }

}

/**
 * Handles Menu System For F1 Statistics App
 */
object MenuHandler {

  /**
   * Prints menu options to the console
   */
  private def displayMenu(): Unit = {
    println(
      """
        |F1 Statistics Application Menu:
        |1. Display winners by year
        |2. Display results for a specific season
        |3. Display total races for each season
        |4. Display average points per season
        |5. Display total points per season
        |6. Display total points for a specific driver
        |7. Exit
        |""".stripMargin
    )
  }

  /**
   * Manages the Menu interaction Loop
   * Displays Menu, Processes Users Input, Maps Input to corresponding action.
   *
   * @param f1Data Dataset used for the F1Statistics Application.
   * @see f1Data
   */
  def menuController(f1Data: F1Data): Unit = {
    val menuOptions = Map(
      1 -> (() => DataAnalyser.displayWinners(f1Data)),
      2 -> (() => DataAnalyser.displaySeasonResults(f1Data)),
      3 -> (() => DataAnalyser.displayTotalRaces(f1Data)),
      4 -> (() => println("Display Average Points for each Season")),
      5 -> (() => println("Display Total Points for each Season")),
      6 -> (() => println("Display Total Points for Specific Driver"))
    )

    /**
     * Handles Recursive Loop For Menu Navigation.
     * Processes User Input, Validates Input, Performs Corresponding Action.
     * Exits Loop when User Selects Exit option
     *
     * @throws IllegalArgumentException If an invalid Menu Option is entered
     */
    @tailrec
    def menuLoop(): Unit = {
      displayMenu()
      StdIn.readLine("Enter Selected Option: ").trim.toIntOption match
        case Some(choice) if menuOptions.contains(choice) =>
          menuOptions(choice)()
          menuLoop()
        case Some(7) =>
          println("Exiting Application...")
          System.exit(0)
        case _ =>
          println("Invalid Option, Please try again.")
          menuLoop()
    }

    // Start Menu Loop
    menuLoop()
  }
}


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

object DataAnalyser {

  /**
   * Extracts the winner (first driver) for each year from the dataset.
   *
   * Assumes the first driver in the list is the winner for that year.
   *
   * @param f1Data The dataset containing F1 data.
   * @return A map where the key is the year and the value is the winner's F1Driver tuple.
   */
  private def getWinners(f1Data: F1Data): Map[Int, F1Driver] = {
    f1Data.collect {
      case (year, drivers) if drivers.nonEmpty => year -> drivers.head
    }
  }

  /**
   * Displays the winners for each year, sorted in descending order by year.
   *
   * @param f1Data The dataset containing F1 data.
   */
  def displayWinners(f1Data: F1Data): Unit = {
    val winners = getWinners(f1Data) // Retrieve the winners for each year

    if (winners.nonEmpty) {
      println("\nWinners by Year:")

      // Print header
      println(f"${"Year"}%-6s ${"Winner"}%-25s ${"Points"}%-10s ${"Wins"}%-5s")
      println("-" * 50)

      // Sort by year and print each winner
      winners.toSeq
        .sortBy(-_._1) // Sort by year in descending order
        .foreach { case (year, (name, points, wins)) =>
          println(f"$year%-6d $name%-25s $points%-10.1f $wins%-5d")
        }
    } else {
      println("\nNo winners data available.")
    }
  }

  /**
   * Retrieves the results for a specific season (year) from the F1 dataset.
   *
   * @param f1Data The dataset containing F1 data.
   * @param year   The year for which results are to be retrieved.
   * @return An Option containing a list of F1Driver tuples if the year exists, or None otherwise.
   */
  private def getSeasonResults(f1Data: F1Data, year: Int): Option[List[F1Driver]] = {
    f1Data.get(year)
  }

  /**
   * Displays the results for a specific season (year), sorted by points and wins.
   *
   * @param f1Data The dataset containing F1 data.
   */
  def displaySeasonResults(f1Data: F1Data): Unit = {
    val year = StdIn.readLine("Enter the year: ").trim.toIntOption
    year match {
      case Some(y) =>
        f1Data.get(y) match {
          case Some(results) =>
            println(s"\nResults for $y:")

            // Print header
            println(f"${"Driver"}%-25s ${"Points"}%-10s ${"Wins"}%-5s")
            println("-" * 45)

            // Format and print driver results
            results.sortBy { case (_, points, wins) => (-points, -wins) } // Sort by points, then wins
              .foreach { case (name, points, wins) =>
                println(f"$name%-25s $points%-10.1f $wins%-5d")
              }
          case None => println(s"No data found for the year $y.")
        }
      case None => println("Invalid year input. Please enter a valid year.")
    }
  }

  /**
   * Calculates the total number of races for each year in the dataset.
   *
   * Iterates through the F1 data, summing the race counts (`_3` field) for all drivers in each year.
   *
   * @param f1Data The dataset containing F1 data.
   * @return A map where the key is the year, and the value is the total number of races for that year.
   */
  private def getTotalRacesByYear(f1Data: F1Data): Map[Int, Int] = {
    f1Data.view.mapValues(drivers => drivers.map(_._3).sum).toMap
  }

  /**
   * Displays the total number of races for each year in the dataset.
   *
   * Retrieves the total races for each year using `getTotalRacesByYear` and formats the output.
   *
   * @param f1Data The dataset containing F1 data.
   */
  def displayTotalRaces(f1Data: F1Data): Unit = {
    val totalRaces = getTotalRacesByYear(f1Data)
    println("\nTotal Races by Year:")

    // Print header
    println(f"${"Year"}%-6s ${"Total Races"}%-12s")
    println("-" * 20)

    // Sort by year in descending order and print each result
    totalRaces.toSeq
      .sortBy(-_._1) // Sort by year in descending order
      .foreach { case (year, races) =>
        println(f"$year%-6d $races%-12d")
      }
  }

  // TODO - Analysis 4 - Get average points per season

  // TODO - Analysis 4 - Display average points per season

  // TODO - Analysis 5 - Get total points by season

  // TODO - Analysis 5 - Display total points by season (ascending)

  // TODO - Analysis 6 - Get total points for a specific driver

  // TODO - Analysis 6 - Display total points for a specific driver
}