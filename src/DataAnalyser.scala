import DataHandler.{F1Data, F1Driver}
import scala.io.StdIn

/**
 * Provides analysis functions for the F1 Statistics App.
 *
 * This handler is responsible for:
 * - Implementing pure functions to extract and compute various statistics from the dataset.
 * - Supporting operations such as:
 *   - Retrieving winners by year.
 *   - Calculating total races, average points, and total points by season.
 *   - Retrieving results for a specific driver or season.
 *     - Separating data operations (pure functions) from user interaction (display functions).
 *
 * Functions:
 * - Pure Functions: Process and compute statistics on `F1Data` without side effects.
 * - Display Functions: Handle user interaction by displaying the results of the pure functions.
 */
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

  /**
   * Calculates the average points for each season in the dataset.
   *
   * @param f1Data The dataset containing F1 data.
   * @return A map where the key is the year and the value is the average points for that season.
   */
  private def getAveragePointsByYear(f1Data: F1Data): Map[Int, Float] = {
    f1Data.map { case (year, drivers) =>
      val totalPoints = drivers.map(_._2).sum
      val averagePoints = if (drivers.nonEmpty) totalPoints / drivers.size else 0.0f
      year -> averagePoints
    }
  }

  /**
   * Displays the average points for each season in the dataset.
   *
   * @param f1Data The dataset containing F1 data.
   */
  def displayAveragePoints(f1Data: F1Data): Unit = {
    val averages = getAveragePointsByYear(f1Data)
    println("\nAverage Points by Year:")

    // Print header
    println(f"${"Year"}%-6s ${"Average Points"}%-15s")
    println("-" * 25)

    // Sort by year in descending order and print each average
    averages.toSeq
      .sortBy(-_._1) // Sort by year in descending order
      .foreach { case (year, avgPoints) =>
        println(f"$year%-6d $avgPoints%-15.2f")
      }
  }


  /**
   * Computes the total points for each season.
   *
   * @param f1Data The dataset containing F1 data.
   * @return A list of tuples where each tuple contains the year and the total points for that season, sorted by year in descending order.
   */
  private def getTotalPointsByYear(f1Data: F1Data): List[(Int, Float)] = {
    f1Data.map { case (year, drivers) =>
      year -> drivers.map(_._2).sum // Calculate the sum of points for each year
    }.toList.sortBy(-_._1) // Sort by year in descending order
  }

  /**
   * Displays the total points for each season, sorted by year in descending order.
   *
   * @param f1Data The dataset containing F1 data.
   */
  def displayTotalPointsByYear(f1Data: F1Data): Unit = {
    val totalPoints = getTotalPointsByYear(f1Data)

    if (totalPoints.nonEmpty) {
      println("\nTotal Points Per Season:")
      // Print header
      println(f"${"Year"}%-6s ${"Total Points"}%-12s")
      println("-" * 20)

      // Print the total points for each season
      totalPoints.foreach { case (year, points) =>
        println(f"$year%-6d $points%-12.1f")
      }
    } else {
      println("\nNo data available to display total points.")
    }
  }

  /**
   * Retrieves the total points for a specific driver or drivers matching a search query.
   *
   * @param f1Data     The dataset containing F1 data.
   * @param driverName The name or partial name of the driver to search for.
   * @return A map where the key is the driver's name and the value is their total points.
   */
  private def getDriverPoints(f1Data: F1Data, driverName: String): Map[String, Float] = {
    f1Data.values.flatten
      .filter { case (name, _, _) => name.toLowerCase.contains(driverName.toLowerCase) }
      .groupBy(_._1)
      .map { case (name, entries) => name -> entries.map(_._2).sum }
  }

  /**
   * Displays the total points for a driver or drivers matching the input query.
   *
   * Prompts the user for a driver name or partial name, retrieves matching drivers,
   * and prints their total points in a formatted table.
   *
   * @param f1Data The dataset containing F1 data.
   */
  def displayDriverPoints(f1Data: F1Data): Unit = {
    val name = StdIn.readLine("Enter the driver's full name or part of the name: ").trim.toLowerCase
    val driverPoints = getDriverPoints(f1Data, name)

    if (driverPoints.isEmpty) {
      println(s"No drivers found matching '$name'.")
    } else {
      println(s"\nTotal Points for Drivers Matching '$name':")

      // Print header
      println(f"${"Driver"}%-20s ${"Total Points"}%-12s")
      println("-" * 35)

      // Print results
      driverPoints.toSeq
        .sortBy(-_._2) // Sort by points in descending order
        .foreach { case (driverName, points) =>
          println(f"$driverName%-20s $points%-12.1f")
        }
    }
  }
}
