import ru.otus.module1.HomeWork.homework_1._

// home_worksheet_1

val list: List[Int] = List(1,2,3,4,5,6)
val newlist = 4 :: list
var reversed = newlist.reverse
val maped = list.map { x => x + 1 }
val filtered = list.filter { x => x % 2 == 0 }

val inclist = List.incList(list)
val shoutString = List.shoutString(List("x", "y", "z"))

val mlist = List(List(1,2,3), List(5,6,7))

val mkstring = list.mkString(",")

