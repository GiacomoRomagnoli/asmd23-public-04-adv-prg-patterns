package scala.u04.monads

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, function1, oneOf}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen, Gen, Properties}
import u04.monads.Monads.Monad
import u04.monads.Optionals.{Optional, given}
import u04.monads.Sequences.{Sequence, given}

object MonadAxioms extends Properties("Monad"):

  // Optionals
  def optMapper[A, B](using cg: Cogen[A], b: Arbitrary[B]): Gen[A => Optional[B]] = function1(opt[B])
  def opt[A](using a: Arbitrary[A]): Gen[Optional[A]] = oneOf(just[A], empty[A])
  def empty[A]: Gen[Optional[A]] = const(Optional.Empty[A]())
  def just[A](using a: Arbitrary[A]): Gen[Optional[A]] = for
    v <- a.arbitrary
  yield Optional.Just(v)

  // Sequences
  def seqMapper[A, B](using cg: Cogen[A], b: Arbitrary[B]): Gen[A => Sequence[B]] = function1(seq[B])
  def seq[A](using a: Arbitrary[A]): Gen[Sequence[A]] = oneOf(nil[A], cons[A])
  def nil[A]: Gen[Sequence[A]] = const(Sequence.Nil[A]())
  def cons[A](using a: Arbitrary[A]): Gen[Sequence[A]] = for
    v <- a.arbitrary
    s <- seq
  yield Sequence.Cons(v, s)

  // Axioms
  property("flatMap(unit(a), f) == f(a)") =
    forAll(arbitrary[Int], optMapper[Int, String]): (a, f) =>
      summon[Monad[Optional]].unit(a).flatMap(f) == f(a)
    &&
    forAll(arbitrary[Int], seqMapper[Int, String]): (a, f) =>
      summon[Monad[Sequence]].unit(a).flatMap(f) == f(a)

  property("flatMap(m,unit(_)) == m") =
    forAll(opt[Int], arbitrary[String]): (m, s) =>
      m.flatMap(_ => summon[Monad[Optional]].unit(s)) == m
    &&
    forAll(seq[Int], arbitrary[String]): (m, s) =>
      m.flatMap(_ => summon[Monad[Sequence]].unit(s)) == m

  property("flatMap(flatMap(m, f), g) == flatMap(m, x => flatMap(f(x), g))") =
    forAll(opt[Int], optMapper[Int, String], optMapper[String, Int]): (m, f, g) =>
      m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
    &&
    forAll(seq[Int], seqMapper[Int, String], seqMapper[String, Int]): (m, f, g) =>
      m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

