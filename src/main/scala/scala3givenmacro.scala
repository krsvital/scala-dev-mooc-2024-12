object scala3givenmacro {
  trait Show[T] {
    inline def show(x:T): String
  }

  case class Foo(x:Int)

  inline given Show[Foo] with {
    inline def show(x:Foo): String = s"!!!! ${x.toString} !!!!"
  }

  @main def scala3givenmacroEx() ={
    println(summon[Show[Foo]].show(Foo(42)))
  }

}