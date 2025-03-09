import CSV._

object Main extends  App {
    case class Car(name: String, id: Int, cost: Double, discount: Boolean)

    val csv = 
        """
            ferarri;1;100000.00;true
            lada;2;2000.00;false
            toyota;3;20000.20;false
        """

    given delim: Delimiter = Delimiter(";")

    CSV[Car](csv).foreach(println)
}
