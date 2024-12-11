package f1statisticsapp

import org.scalatest.funsuite.AnyFunSuite
import f1statisticsapp.handlers.DataHandler
import f1statisticsapp.analysis.DataAnalyser
import org.scalatest.SequentialNestedSuiteExecution

class ApplicationIntegrationTest extends AnyFunSuite with SequentialNestedSuiteExecution {


  // ------
  test("Application should load data successfully") {
    println("=== Starting test: Application should load data successfully ===")
    Console.flush()
    try {
      println("Attempting to load data from 'resources/data.txt'")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println(s"Loaded data: $data") // Log the loaded data for verification
      Console.flush()

      assert(data.nonEmpty, "Data should not be empty after successful load.")
      println("=== Test passed: Data loaded and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
  // ------
  test("Application should handle invalid file paths gracefully") {
    println("=== Starting test: Application should handle invalid file paths gracefully ===")
    Console.flush()

    var data: Option[Any] = None // Placeholder for the data variable
    try {
      println("Attempting to load data from invalid path 'invalid/path.txt'")
      Console.flush()

      data = Some(DataHandler.loadDataFromFile("invalid/path.txt")) // This should throw an exception
      fail("Expected IllegalArgumentException to be thrown, but no exception was thrown.")
    } catch {
      case ex: IllegalArgumentException =>
        println(s"Intercepted exception: ${ex.getMessage}")
        Console.flush()

        // Assert that the exception message is as expected
        assert(
          ex.getMessage.toLowerCase.contains("file not found"),
          "Expected exception message should indicate the file was not found."
        )

        println("=== Test passed: Invalid file path handled correctly ===\n")
        Console.flush()
      case ex: Exception =>
        println(s"Unexpected exception type thrown: ${ex.getClass.getName} - ${ex.getMessage}")
        Console.flush()
        fail("Unexpected exception type.")
    }

    // Optionally verify that `data` remains None
    assert(data.isEmpty, "Data should not be loaded for an invalid file path.")
  }
  // ------
  test("Winners should be extracted correctly") {
    println("=== Starting test: Winners should be extracted correctly ===")
    Console.flush()
    try {
      println("Loading data...")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println("Extracting winners...")
      Console.flush()

      val winners = DataAnalyser.getWinners(data)
      println(s"Winners extracted: $winners")
      Console.flush()

      assert(winners.nonEmpty, "Winners map should not be empty.")
      assert(winners.values.forall(driver => driver._2 > 0), "All winners should have non-zero points.")
      println("=== Test passed: Winners extracted and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
  // ------
  test("Season results should be retrieved correctly") {
    println("=== Starting test: Season results should be retrieved correctly ===")
    Console.flush()
    try {
      println("Loading data...")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println("Retrieving season results for 2020...")
      Console.flush()

      val seasonResults = DataAnalyser.getSeasonResults(data, 2020)
      println(s"Season results for 2020: $seasonResults")
      Console.flush()

      assert(seasonResults.isDefined, "Season results should be defined for a valid year.")
      assert(seasonResults.get.nonEmpty, "Season results should not be empty.")
      println("=== Test passed: Season results retrieved and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
  // ------
  test("Application should calculate total races correctly") {
    println("=== Starting test: Application should calculate total races correctly ===")
    Console.flush()
    try {
      println("Loading data...")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println("Calculating total races by year...")
      Console.flush()

      val totalRaces = DataAnalyser.getTotalRacesByYear(data)
      println(s"Total races by year: $totalRaces")
      Console.flush()

      assert(totalRaces.nonEmpty, "Total races map should not be empty.")
      assert(totalRaces.values.forall(_ > 0), "Each year should have a positive number of races.")
      println("=== Test passed: Total races calculated and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
  // ------
  test("Application should calculate average points correctly") {
    println("=== Starting test: Application should calculate average points correctly ===")
    Console.flush()
    try {
      println("Loading data...")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println("Calculating average points by year...")
      Console.flush()

      val avgPoints = DataAnalyser.getAveragePointsByYear(data)
      println(s"Average points by year: $avgPoints")
      Console.flush()

      assert(avgPoints.nonEmpty, "Average points map should not be empty.")
      assert(avgPoints.values.forall(_ >= 0), "Average points should be non-negative.")
      println("=== Test passed: Average points calculated and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
  // ------
  test("Application should calculate total points correctly") {
    println("=== Starting test: Application should calculate total points correctly ===")
    Console.flush()
    try {
      println("Loading data...")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println("Calculating total points by year...")
      Console.flush()

      val totalPoints = DataAnalyser.getTotalPointsByYear(data)
      println(s"Total points by year: $totalPoints")
      Console.flush()

      assert(totalPoints.nonEmpty, "Total points list should not be empty.")
      assert(totalPoints.forall { case (_, points) => points >= 0 }, "Total points should be non-negative.")
      println("=== Test passed: Total points calculated and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
  // ------
  test("Application should find driver points correctly") {
    println("=== Starting test: Application should find driver points correctly ===")
    Console.flush()
    try {
      println("Loading data...")
      Console.flush()

      val data = DataHandler.loadDataFromFile("resources/data.txt")
      println("Finding points for driver 'Hamilton'...")
      Console.flush()

      val driverPoints = DataAnalyser.getDriverPoints(data, "Hamilton")
      println(s"Driver points for 'Hamilton': $driverPoints")
      Console.flush()

      assert(driverPoints.nonEmpty, "Driver points map should not be empty for valid input.")
      assert(driverPoints.keys.exists(_.toLowerCase.contains("hamilton")), "Results should include 'Hamilton'.")
      println("=== Test passed: Driver points found and verified successfully ===")
      Console.flush()
    } catch {
      case ex: Exception =>
        println(s"=== Test failed: Exception encountered - ${ex.getMessage} ===")
        Console.flush()
        throw ex
    }
  }
}