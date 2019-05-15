package json

import shapeless._
import shapeless.labelled.{FieldType, field}

sealed abstract class AST
//sealed abstract class Nullable[A <: AST] extends AST
//case object
case object Null extends AST
case class Bool(value: Boolean) extends AST
case class Str(value: String) extends AST
case class Num(value: Double) extends AST
case class Arr(value: List[AST]) extends AST
case class Obj(value: Map[String, AST]) extends AST {
  def addEntry(pair: (String, AST)) = Obj(value + pair)
}
trait Codec[A] {
  def encode(a: A): AST
  def decode(json: AST): A

  def dimap[B](enc: B => A, dec: A => B): Codec[B] =
    Codec.instance[B](enc andThen encode, dec compose decode)

}

trait ObjectCodec[A] extends Codec[A] {
  def encode(a: A): Obj
}

object Codec {

  def apply[A](implicit codec: Codec[A]) = codec

  def instance[A](enc: A => AST, dec: AST => A): Codec[A] = new Codec[A] {
    def encode(a: A): AST = enc(a)
    def decode(json: AST): A = dec(json)
  }

  // Manually-defined base base instances
  implicit val boolInstance = instance[Boolean](Bool(_), _.asInstanceOf[Bool].value)
  implicit val stringInstance = instance[String](Str(_), _.asInstanceOf[Str].value)
  implicit val doubleInstance = instance[Double](Num(_), _.asInstanceOf[Num].value)
  implicit val intInstance = doubleInstance.dimap[Int](_.toDouble, _.toInt)
  implicit def arrayInstance[A](implicit elemCodec: Codec[A])= instance[List[A]](
    l => Arr(l map elemCodec.encode),
    a => a.asInstanceOf[Arr].value map elemCodec.decode
  )
  implicit def mapInstance[A](implicit elemCodec: Codec[A])= new ObjectCodec[Map[String, A]] {
    def encode(m: Map[String, A]) = Obj(m mapValues elemCodec.encode)
    def decode(o: AST) = o.asInstanceOf[Obj].value mapValues elemCodec.decode
  }
  implicit def optionInstance[A](implicit elemCodec: Codec[A]) = instance[Option[A]](
    _.fold[AST](Null)(elemCodec.encode),
    {
      case Null => None
      case j => Some(elemCodec.decode(j))
    }
  )

  // Shapeless instance for products
  implicit object decodeHNil extends ObjectCodec[HNil] {
    def encode(a: HNil): Obj = Obj(Map.empty)
    def decode(json: AST): HNil = HNil
  }
  implicit def decodeHCons[K <: Symbol, V, T <: HList](implicit key: Witness.Aux[K], vc: Codec[V], tc: ObjectCodec[T]) = new ObjectCodec[FieldType[K, V] :: T] {
    def encode(a: FieldType[K, V] :: T): Obj = tc.encode(a.tail).addEntry(key.value.name -> vc.encode(a.head))
    def decode(json: AST) = field[K](vc.decode(json.asInstanceOf[Obj].value(key.value.name))) :: tc.decode(json)
  }

  implicit def genericCodec[A, Repr](implicit gen: LabelledGeneric.Aux[A, Repr], reprCodec: ObjectCodec[Repr]) = new ObjectCodec[A] {
    def encode(a: A) = reprCodec.encode(gen.to(a))
    def decode(json: AST): A = gen.from(reprCodec.decode(json))
  }

}

