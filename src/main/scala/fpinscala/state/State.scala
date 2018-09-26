package fpinscala.state

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(st => {
      val (a, s) = run(st)
      f(a).run(s)
    })
  }
}

object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(rng => (a, rng))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.reverse.foldLeft[State[S, List[A]]](unit(Nil))((acc, f) => f.map2(acc)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}