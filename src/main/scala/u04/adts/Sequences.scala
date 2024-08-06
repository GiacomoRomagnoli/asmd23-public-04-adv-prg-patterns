package u04.adts

import u04.monads.Optionals.Optional.Just

import scala.annotation.tailrec

object Sequences:

  trait SequenceADT:
    type Sequence[A]
// constructors:
    def cons[A](a: A, s: Sequence[A]): Sequence[A]
    def nil[A](): Sequence[A]
// operations:
    extension [A](seq: Sequence[A])
      def map[B](mapper: A => B): Sequence[B]
      def filter(f: A => Boolean): Sequence[A]
      def flatMap[B](mapper: A => Sequence[B]): Sequence[B]
      def append (seq2: Sequence[A] ): Sequence[A]
      def foldLeft[B](acc: B)(op: (B, A) => B): B
      def foldRight[B](acc: B)(op: (A, B) => B): B
      def reduce(op: (A, A) => A): A
      def head: Option[A]
      def tail: Sequence[A]
/* axioms:
      map(nil, f) = nil
      map(cons(h, t), f) = cons(f(h), map(t, f))

      filter(nil, f) = nil
      filter(cons(h, t), f) = if f(h) then cons(h, filter(t, f)) else filter(t, f)
      
      flatMap(nil, f) = nil
      flatMap(cons(h, t), f) = append(f(h), flatMap(t, f))

      append(nil, l) = l
      append(cons(h, t), l) = cons(h, append(t, l))
      
      foldLeft(nil, acc, f) = acc
      foldLeft(cons(h, t), acc, f) = foldLeft(t, f(acc, h), f)

      foldRight(nil, acc, f) = acc
      foldRight(cons(h, t), acc, f) = f(h, foldRight(t, acc, f))

      reduce(cons(h, t), f) = foldLeft(t, h, f)
*/

  // Scala List implementation \\
  object ProxySequence extends SequenceADT:
    override opaque type Sequence[A] = List[A]

    override def cons[A](a: A, s: Sequence[A]): Sequence[A] = a :: s
    override def nil[A](): Sequence[A] = List()

    extension [A](seq: Sequence[A])
      override def map[B](mapper: A => B): Sequence[B] = seq.map(mapper)
      override def filter(f: A => Boolean): Sequence[A] = seq.filter(f)
      override def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = seq.flatMap(mapper)
      override def append(seq2: Sequence[A]): Sequence[A] = seq ++ seq2
      override def foldLeft[B](acc: B)(op: (B, A) => B): B = seq.foldLeft(acc)(op)
      override def foldRight[B](acc: B)(op: (A, B) => B): B = seq.foldRight(acc)(op)
      override def reduce(op: (A, A) => A): A = seq.reduce(op)
      override def head: Option[A] = seq.headOption
      override def tail: Sequence[A] = seq.drop(1)


  // Cons|Nil implementation \\
  object Sequence extends SequenceADT:
    override opaque type Sequence[A] = SequenceImpl[A]

    private enum SequenceImpl[E]:
      case Cons(head: E, tail: Sequence[E])
      case Nil()

    import SequenceImpl.*

    override def cons[E](head: E, tail: Sequence[E]): Sequence[E] = Cons(head, tail)
    override def nil[E](): Sequence[E] = Nil[E]()

    extension [A](seq: Sequence[A])
      override def map[B](mapper: A => B): Sequence[B] =
        seq.flatMap(h => Cons(mapper(h), Nil()))

      override def filter(f: A => Boolean): Sequence[A] = seq match
        case Cons(h, t) if f(h) => Cons(h, t.filter(f))
        case Cons(_, t) => t.filter(f)
        case Nil() => Nil()

      override def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = seq match
        case Cons(h, t) => mapper(h).append(t.flatMap(mapper))
        case Nil() => Nil()

      override def append(seq2: Sequence[A]): Sequence[A] = seq match
        case Cons(h, t) => Cons(h, t.append(seq2))
        case Nil() => seq2

      @tailrec
      override def foldLeft[B](acc: B)(op: (B, A) => B): B = seq match
        case Cons(h, t) => t.foldLeft(op(acc, h))(op)
        case Nil() => acc

      override def foldRight[B](acc: B)(op: (A, B) => B): B = seq match
        case Cons(h, t) => op(h, t.foldRight(acc)(op))
        case Nil() => acc

      override def reduce(op: (A, A) => A): A = seq match
        case Cons(h, t) => t.foldLeft(h)(op)
        case Nil() => throw UnsupportedOperationException()
        
      override def head: Option[A] = seq match
        case Cons(h, t) => Some(h)
        case Nil() => None
        
      override def tail: Sequence[A] = seq match
        case Cons(h, t) => t
        case _ => Nil()