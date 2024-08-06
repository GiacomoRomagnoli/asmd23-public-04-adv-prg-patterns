package u04.atds

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen.{const, function1, oneOf}
import org.scalacheck.{Arbitrary, Cogen, Gen, Properties}
import org.scalacheck.Arbitrary.arbitrary
import u04.adts
import u04.adts.Sequences.*

object SequenceAxioms extends Properties("SequenceADT"):
  given adt: Arbitrary[SequenceADT] = Arbitrary(oneOf(Sequence, ProxySequence))
  def mapper[A, B](adt: SequenceADT)(using b: Arbitrary[B], a: Cogen[A]): Gen[A => adt.Sequence[B]] = function1(seq[B](adt))
  def seq[A](adt: SequenceADT)(using a: Arbitrary[A]): Gen[adt.Sequence[A]] = oneOf(nil[A](adt), cons[A](adt))
  def nil[A](adt: SequenceADT): Gen[adt.Sequence[A]] = const(adt.nil[A]())
  def cons[A](adt: SequenceADT)(using a: Arbitrary[A]): Gen[adt.Sequence[A]] = for
    v <- a.arbitrary
    s <- seq(adt)
  yield adt.cons(v, s)

  property("map(nil, f) = nil") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(nil[Int](adt), arbitrary[Int => String]): (seq, f) =>
        adt.map(seq)(f) == adt.nil()

  property("map(cons(h, t), f) = cons(f(h), map(t, f))") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[Int](adt), arbitrary[Int => String]): (seq, f) =>
        adt.map(seq)(f) == adt.cons(f(seq.head.get), adt.map(seq.tail)(f))

  property("filter(nil, f) = nil") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(nil[Int](adt), arbitrary[Int => Boolean]): (seq, f) =>
        adt.filter(seq)(f) == adt.nil()

  property("filter(cons(h, t), f) = if f(h) then cons(h, filter(t, f)) else filter(t, f)") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[Int](adt), arbitrary[Int => Boolean]): (seq, f) =>
        adt.filter(seq)(f) ==
          (if f(seq.head.get) then adt.cons(seq.head.get, adt.filter(seq.tail)(f)) else adt.filter(seq.tail)(f))

  property("flatMap(nil, f) = nil") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(nil[Int](adt), mapper[Int, String](adt)): (seq, f) =>
        adt.flatMap(seq)(f) == adt.nil()

  property("flatMap(cons(h, t), f) = append(f(h), flatMap(t, f))") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[Int](adt), mapper[Int, String](adt)): (seq, f) =>
        adt.flatMap(seq)(f) == adt.append(f(seq.head.get))(adt.flatMap(seq.tail)(f))

  property("append(nil, l) = l") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(nil[Int](adt), seq[Int](adt)): (seq1, seq2) =>
        adt.append(seq1)(seq2) == seq2

  property("append(cons(h, t), l) = cons(h, concat(t, l))") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[Int](adt), seq[Int](adt)): (seq1, seq2) =>
        adt.append(seq1)(seq2) == adt.cons(seq1.head.get, adt.append(seq1.tail)(seq2))

  property("foldLeft(nil, acc, f) = acc") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(nil[String](adt), arbitrary[Int], arbitrary[(Int, String) => Int]): (seq, acc, f) =>
        adt.foldLeft(seq)(acc)(f) == acc
        
  property("foldLeft(cons(h, t), acc, f) = foldLeft(t, f(acc, h), f)") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[String](adt), arbitrary[Int], arbitrary[(Int, String) => Int]): (seq, acc, f) =>
        adt.foldLeft(seq)(acc)(f) == adt.foldLeft(seq.tail)(f(acc, seq.head.get))(f)
        
  property("foldRight(nil, acc, f) = acc") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(nil[String](adt), arbitrary[Int], arbitrary[(String, Int) => Int]): (seq, acc, f) =>
        adt.foldRight(seq)(acc)(f) == acc
        
  property("foldRight(cons(h, t), acc, f) = f(h, foldRight(t, acc, f))") = 
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[String](adt), arbitrary[Int], arbitrary[(String, Int) => Int]): (seq, acc, f) =>
        adt.foldRight(seq)(acc)(f) == f(seq.head.get, adt.foldRight(seq.tail)(acc)(f))  
        
  property("reduce(cons(h, t), f) = foldLeft(t, h, f)") =
    forAll(arbitrary[SequenceADT]): adt =>
      forAll(cons[String](adt), arbitrary[(String, String) => String]): (seq, f) =>
        adt.reduce(seq)(f) == adt.foldLeft(seq.tail)(seq.head.get)(f)