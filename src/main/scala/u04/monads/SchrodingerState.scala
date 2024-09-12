package scala.u04.monads

import u04.monads.States.State
import scala.util.Random

trait SchrodingerState:
  type Cat
  def pick: Cat
  def guess(c: Cat): State[Cat, String]
  def nop: State[Cat, Unit]

object SchrodingerStateImpl extends SchrodingerState:
  override opaque type Cat = Int

  override def pick: Cat = Random.nextInt(11)

  override def guess(c: Int): State[Cat, String] =
    State:
      case s if s == c => (s, "guessed")
      case s if s < c => (s, "too high")
      case s if s > c => (s, "too low")

  override def nop: State[Cat, Unit] = State(s => (s, ()))

@main def testSchrodingerState(): Unit =
  import SchrodingerStateImpl.*

  val game = for
    x <- guess(3)
  yield x

  println(game.run(pick))