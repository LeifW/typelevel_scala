package nats

// List[A] = Nil | Cons(a: A, List[A])

// Nat = Zero | Succ(n: Nat)

/*
sealed trait Nat {
  def +(n: Nat): Nat
}

case object Zero extends Nat {
  def +(n: Nat): Nat = n
}
case class Succ(n: Nat) extends Nat {
  def +(n: Nat): Nat = Succ(this.n + n)
}
*/


sealed trait Nat {
  type plus[M <: Nat] <: Nat
}
class Zero extends Nat {
  type plus[M <: Nat] = M
}
class Succ[N <: Nat] extends Nat {
  type plus[M <: Nat] = Succ[N#plus[M]]
}

object Foo {
  type one = Succ[Zero]
  type two = Succ[one]
  type three = two#plus[one]
  implicitly[three =:= Succ[Succ[Succ[Zero]]]]
}

sealed trait NList[N <: Nat, A]
case class Nil[A]() extends NList[Zero, A]
case class Cons[N <: Nat, A](a: A, tail: NList[N, A]) extends NList[Succ[N], A]
