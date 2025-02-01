package object monad {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   * HINT: для проверки на пустой элемент можно использовать eq
   */

  trait Wrap[+A] {

    def get: A

    def pure[R](x: R): Wrap[R] = Wrap(x)

    def flatMap[R](f: A => Wrap[R]): Wrap[R] = f(get)

    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = flatMap(a => pure(f(a)))

    def withFilter(f: A => Boolean): Wrap[A] = get match {
      case a if f(a) => Wrap(a)
      case _ => Wrap.empty
    }
  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
    def apply[A](a: A): Wrap[A] = a match {
      case null => EmptyWrap
      case x => NonEmptyWrap(x)
    }
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
  } // bottom, null element

}