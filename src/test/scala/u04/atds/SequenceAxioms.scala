package u04.atds

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen.{const, oneOf}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Arbitrary.arbitrary
import u04.adts
import u04.adts.Sequences.*

object SequenceAxioms extends Properties("SequenceADT"):
  given adt: Arbitrary[SequenceADT] = Arbitrary(oneOf(Sequence, ProxySequence))
  val f: Int => Int = _ + 1

  property("map(nil, f) = nil") =
    forAll(arbitrary[SequenceADT]): adt =>
      adt.map(adt.nil())(_.toString) == adt.nil()

  property("map(cons(h, t), f) = cons(f(h), map(t, f))") =
    forAll(arbitrary[SequenceADT]): adt =>
      adt.map(adt.cons(1, adt.cons(2, adt.nil())))(f) == adt.cons(f(1), adt.map(adt.cons(2, adt.nil()))(f))
