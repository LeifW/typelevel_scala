package list

sealed trait TList
class TNil extends TList
class TCons[A, T <: TList] extends TList

sealed trait HList[T <: TList]
case object HNil extends HList[TNil]
case class HCons[A, T <: TList](a: A, tail: HList[T]) extends HList[TCons[A, T]]

object Foo {
  type StringAndInt = TCons[String, TCons[Int, TNil]]
}