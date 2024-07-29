package u04.adts

object SequenceADT:
  // implementation of data structure: fully hidden
  private enum SequenceImpl[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()
  import SequenceImpl.* 

  // the type Sequence is an opaque alias to SequenceImpl
  opaque type Sequence[A] = SequenceImpl[A]

  // constructors
  def cons[E](head: E, tail: Sequence[E]): Sequence[E] = Cons(head, tail)
  def nil[E](): Sequence[E] = Nil[E]()

  // operation as extension methods
  extension [A](seq: Sequence[A])
    def map[B](mapper: A => B): Sequence[B] = seq match
      case Cons(h, t) => Cons(mapper(h), t.map(mapper))
      case Nil()      => Nil()

    def filter(f: A => Boolean): Sequence[A] = seq match
      case Cons(h, t) if f(h) => Cons(h, t.filter(f))
      case Cons(h, t) => t.filter(f)
      case Nil() => Nil()

  // operations as standard methods
  def concat[A](seq1: Sequence[A], seq2: Sequence[A]): Sequence[A] = seq1 match
    case Cons(h, t) => Cons(h, concat(t, seq2))
    case Nil()      => seq2

@main def trySequenceADT(): Unit =
  import SequenceADT.* 
  val seq = cons(10, cons(20, cons(30, nil())))
  println(concat(seq, seq.map(_ + 1)))