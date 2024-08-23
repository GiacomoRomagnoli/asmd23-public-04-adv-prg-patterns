package scala.u04.monads

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, function1, oneOf}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen, Gen, Properties}
import u04.monads.Monads.Monad
import u04.monads.Optionals.{Optional, given}
import u04.monads.Sequences
import u04.monads.Sequences.{Sequence, given}

import scala.reflect.ClassTag

object MonadAxioms extends Properties("Monad"):
  // Monads
  def monad[T[_]: Monad]: Gen[Monad[T]] = summon[Monad[T]]
  // Optionals
  def opt[A](using a: Arbitrary[A]): Gen[Optional[A]] = oneOf(just[A], empty[A])
  def empty[A]: Gen[Optional[A]] = const(Optional.Empty[A]())
  def just[A](using a: Arbitrary[A]): Gen[Optional[A]] = for
    v <- a.arbitrary
  yield Optional.Just(v)
  // Sequences
  def seq[A](using a: Arbitrary[A]): Gen[Sequence[A]] = oneOf(nil[A], cons[A])
  def nil[A]: Gen[Sequence[A]] = const(Sequence.Nil[A]())
  def cons[A](using a: Arbitrary[A]): Gen[Sequence[A]] = for
    v <- a.arbitrary
    s <- seq
  yield Sequence.Cons(v, s)

  //property("flatMap(unit(a), f) == f(a)") =
    //forAll(oneOf(Sequence, Optional)): t =>



