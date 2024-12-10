import scala.annotation.tailrec
import scala.io.StdIn


object F1StatisticsApp extends App {

  @main
  def main(): Unit = {
    println("Welcome to the F1 Statistics CLI Application")

    MenuHandler.menuController()

    // TODO -  Load f1 Dataset

    // TODO - Check Dataset loaded (fail/pass)

    // TODO - Pass - Load Menu

    // TODO - Fail - Display Message and Exit
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
  def menuController(): Unit = {
    val menuOptions = Map(
      1 -> (() => println("Display Winners By Year")),
      2 -> (() => println("Display Results for Specific Season")),
      3 -> (() => println("Display Total Races for each season")),
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

  // TODO - Create type aliases for F1Data and F1Driver

  // TODO - Load Data from file

  // TODO - Parse F1 Data
}

object DataAnalyser {

  // TODO - Analysis 1 - Get winners by year

  // TODO - Analysis 1 - Display winners by year

  // TODO - Analysis 2 - Get results for a specific season

  // TODO - Analysis 2 - Display results for a specific season

  // TODO - Analysis 3 - Get total number of races each season/year

  // TODO - Analysis 3 - Display total number of races each season/year

  // TODO - Analysis 4 - Get average points per season

  // TODO - Analysis 4 - Display average points per season

  // TODO - Analysis 5 - Get total points by season

  // TODO - Analysis 5 - Display total points by season (ascending)

  // TODO - Analysis 6 - Get total points for a specific driver

  // TODO - Analysis 6 - Display total points for a specific driver
}