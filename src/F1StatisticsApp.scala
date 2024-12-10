/**
 * Entry point for the F1 Statistics CLI Application.
 *
 * This singleton object:
 * - Initializes the application by loading the dataset.
 * - Handles any errors during initialization (e.g., missing files, malformed data).
 * - Invokes the `MenuHandler` to display the menu and handle user input.
 *
 * Workflow:
 * 1. Load the dataset using `DataHandler`.
 * 2. Validate the dataset.
 * 3. Display the main menu and delegate user interactions to `MenuHandler`.
 */
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