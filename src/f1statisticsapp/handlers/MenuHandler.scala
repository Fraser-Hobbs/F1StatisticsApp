package f1statisticsapp.handlers

import f1statisticsapp.analysis.DataAnalyser
import f1statisticsapp.handlers.DataHandler.F1Data

import scala.annotation.tailrec
import scala.io.StdIn

/**
 * Manages the interactive CLI menu for the F1 Statistics Application.
 *
 * This handler is responsible for:
 * - Displaying a structured menu of options to the user.
 * - Mapping menu options to corresponding analysis functions from `DataAnalyser`.
 * - Handling user input, including validating choices and invoking the correct operations.
 * - Providing a tail-recursive loop to maintain interaction until the user chooses to exit.
 *
 * Responsibilities:
 * - Ensures separation of user interaction logic from the data processing logic.
 * - Handles invalid menu choices and provides user feedback.
 * - Delegates analysis tasks to `DataAnalyser` for pure operations.
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
  def menuController(f1Data: F1Data, exitFunction: () => Unit = () => System.exit(0)): Unit = {
    val menuOptions = Map(
      1 -> (() => DataAnalyser.displayWinners(f1Data)),
      2 -> (() => DataAnalyser.displaySeasonResults(f1Data)),
      3 -> (() => DataAnalyser.displayTotalRaces(f1Data)),
      4 -> (() => DataAnalyser.displayAveragePoints(f1Data)),
      5 -> (() => DataAnalyser.displayTotalPointsByYear(f1Data)),
      6 -> (() => DataAnalyser.displayDriverPoints(f1Data))
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
          exitFunction()
        case _ =>
          println("Invalid Option, Please try again.")
          menuLoop()
    }

    // Start Menu Loop
    menuLoop()
  }
}
