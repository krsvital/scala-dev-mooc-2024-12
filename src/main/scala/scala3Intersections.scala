// 1. intersection

trait Test1:
  def reset():Unit


trait Test2[T]:
  def add(t: T): Unit

def f(x: Test1 & Test2[String]) =
  x.reset()
  x.add("sdf")
  
// union
case class UserName(name: String)
case class Password(pass:Int)

val password = Password(123)
val userName = UserName("123")
val either: Password | UserName = if true then password else userName
