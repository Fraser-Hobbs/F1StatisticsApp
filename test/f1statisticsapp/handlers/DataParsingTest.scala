package f1statisticsapp.handlers

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths

/**
 * Test suite for the DataHandler functionality.
 *
 * This class validates:
 * - Loading data from files.
 * - Handling malformed data gracefully.
 * - Ensuring proper parsing of F1 statistics data.
 */
class DataParsingTest extends AnyFunSuite {

  private val validFilePath: String = Paths.get("resources/data.txt").toAbsolutePath.toString

  /**
   * Validate successful data loading.
   */
  test("Load data from valid file") {
    val data = DataHandler.loadDataFromFile(validFilePath)
    assert(data.nonEmpty, "Data should not be empty when loaded from a valid file.")
    assert(data.keySet.nonEmpty, "The dataset should contain keys representing years.")
  }

  /**
   * Validate exception for missing file.
   */
  test("Throw exception for missing file") {
    intercept[IllegalArgumentException] {
      DataHandler.loadDataFromFile("resources/nonexistent.txt")
    }
  }

  /**
   * Validate exclusion of malformed lines.
   */
  test("Exclude malformed lines") {
    val malformedData = List(
      "2023,Max Verstappen 575 19,Sergio Perez: 285 2", // Missing colon for first driver
      "Invalid Line Format", // Totally malformed line
      "2022,Max Verstappen: 454 15" // Valid data
    )

    val parsedData = DataHandler.parseDataset(malformedData)
    assert(parsedData.contains(2022), "Year 2022 should be parsed successfully.")
    assert(!parsedData.contains(2023), "Year 2023 should not be parsed due to malformed driver data.")
  }

  /**
   * Validate dataset parsing.
   */
  test("Parse valid dataset") {
    val validData = List(
      "2023,Max Verstappen: 575 19,Sergio Perez: 285 2",
      "2022,Max Verstappen: 454 15,Charles Leclerc: 308 3"
    )

    val parsedData = DataHandler.parseDataset(validData)
    assert(parsedData.contains(2023), "Year 2023 should be present in the parsed dataset.")
    assert(parsedData(2023).nonEmpty, "Parsed data for 2023 should contain driver entries.")
    assert(parsedData(2023).exists(_._1 == "Max Verstappen"), "Max Verstappen should be present for 2023.")
  }

  /**
   * Validate handling of empty files.
   */
  test("Handle empty file") {
    val emptyData = List()
    val parsedData = DataHandler.parseDataset(emptyData)
    assert(parsedData.isEmpty, "Parsed data should be empty for an empty file.")
  }

  /**
   * Validate parsing of individual driver entry.
   */
  test("Parse valid driver entry") {
    val driverEntry = "Max Verstappen: 575 19"
    val parsedDriver = DataHandler.parseDriver(driverEntry)
    assert(parsedDriver._1 == "Max Verstappen", "Driver name should be parsed correctly.")
    assert(parsedDriver._2 == 575.0f, "Driver points should be parsed correctly.")
    assert(parsedDriver._3 == 19, "Driver wins should be parsed correctly.")
  }

  /**
   * Validate handling of malformed driver entry.
   */
  test("Handle malformed driver entry") {
    val malformedEntry = "Max Verstappen 575 19" // Missing colon
    val parsedDriver = DataHandler.parseDriver(malformedEntry)
    assert(parsedDriver._1 == "Unknown", "Malformed driver entry should return 'Unknown'.")
    assert(parsedDriver._2 == 0.0f, "Malformed driver entry should have 0 points.")
    assert(parsedDriver._3 == 0, "Malformed driver entry should have 0 wins.")
  }
}