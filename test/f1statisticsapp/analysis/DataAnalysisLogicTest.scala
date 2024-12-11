package f1statisticsapp.analysis

import org.scalatest.funsuite.AnyFunSuite
import f1statisticsapp.handlers.DataHandler.{F1Data, F1Driver}

class DataAnalysisLogicTest extends AnyFunSuite {

  // Sample dataset for testing
  private val sampleData: F1Data = Map(
    2023 -> List(("Max Verstappen", 575.0f, 19), ("Sergio Perez", 285.0f, 2)),
    2022 -> List(("Max Verstappen", 454.0f, 15), ("Charles Leclerc", 308.0f, 3))
  )

  test("getWinners should return the correct winners for each year") {
    val expected = Map(
      2023 -> ("Max Verstappen", 575.0f, 19),
      2022 -> ("Max Verstappen", 454.0f, 15)
    )
    val result = DataAnalyser.getWinners(sampleData)
    assert(result == expected, s"Expected $expected but got $result")
  }

  test("getSeasonResults should return results for a specific year") {
    val expected2023 = Some(List(("Max Verstappen", 575.0f, 19), ("Sergio Perez", 285.0f, 2)))
    val result2023 = DataAnalyser.getSeasonResults(sampleData, 2023)
    assert(result2023 == expected2023, s"Expected $expected2023 but got $result2023")

    val expected2021 = None
    val result2021 = DataAnalyser.getSeasonResults(sampleData, 2021)
    assert(result2021 == expected2021, s"Expected $expected2021 but got $result2021")
  }

  test("getTotalRacesByYear should return the total number of races for each year") {
    val expected = Map(
      2023 -> 21,
      2022 -> 18
    )
    val result = DataAnalyser.getTotalRacesByYear(sampleData)
    assert(result == expected, s"Expected $expected but got $result")
  }

  test("getAveragePointsByYear should return the average points for each year") {
    val expected = Map(
      2023 -> 430.0f,
      2022 -> 381.0f
    )
    val result = DataAnalyser.getAveragePointsByYear(sampleData)
    assert(result == expected, s"Expected $expected but got $result")
  }

  test("getTotalPointsByYear should return the total points for each year") {
    val expected = List(
      2023 -> 860.0f,
      2022 -> 762.0f
    )
    val result = DataAnalyser.getTotalPointsByYear(sampleData)
    assert(result == expected, s"Expected $expected but got $result")
  }

  test("getDriverPoints should return the total points for drivers matching the name") {
    val expected = Map("Max Verstappen" -> 1029.0f)
    val result = DataAnalyser.getDriverPoints(sampleData, "Max Verstappen")
    assert(result == expected, s"Expected $expected but got $result")

    val partialNameExpected = Map(
      "Max Verstappen" -> 1029.0f,
      "Sergio Perez" -> 285.0f,
      "Charles Leclerc" -> 308.0f
    )
    val partialResult = DataAnalyser.getDriverPoints(sampleData, "e")
    assert(partialResult == partialNameExpected, s"Expected $partialNameExpected but got $partialResult")

    val noMatchExpected = Map.empty[String, Float]
    val noMatchResult = DataAnalyser.getDriverPoints(sampleData, "Unknown Driver")
    assert(noMatchResult == noMatchExpected, s"Expected $noMatchExpected but got $noMatchResult")
  }
}