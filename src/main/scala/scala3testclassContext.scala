class GivenInt(using val usingParam: String)(using val usingParam1: String){}

object scala3testclassContext {
  @main def scala3testclassContextEx() = {

    val b = GivenInt(using "42")(using "test")
    import b.given


    println(usingParam)
    //println(usingParam1)
  }


}