package u04.monads

import u04.monads.States.State

def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] =
  State:
    case (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

@main def runMVC =
  import Monads.*, Monad.*, States.*, State.*, CounterStateImpl.*, WindowStateImpl.*
  import u04.datastructures.Streams.*


  def windowCreation(str: String): State[Window, Stream[String]] = for 
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(seq(reset(), get()), i => windowCreation(i.toString()))
    _ <- seqN(events.map(_ match
        case "IncButton" => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
        case "DecButton" => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
        case "ResetButton" => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
        case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
  yield ()

  controller.run((initialCounter(), initialWindow))

@main def drawNumberGame =
  import WindowStateImpl.*
  import scala.u04.monads.SchrodingerStateImpl.{guess, nop, pick}
  import u04.monads.Monads.Monad.seqN

  val view = for
    _ <- setSize(300, 300)
    _ <- addButton("quit", "QuitButton")
    _ <- addSpinner(0, 0, 10, 1, "number")
    _ <- addButton("guess", "GuessButton")
    _ <- addLabel("can you guess it?", "OutPutLabel")
    _ <- show()
    events <- eventStream()
  yield events

  // per dare un numero di vite potrei fare in modo che lo stato di
  // Schrodinger abbia come effetto una tupla Stringa Intero
  val controller = for
    e <- mv(nop, _ => view)
    _ <- seqN(e.map {
      case "QuitButton" => mv(nop, _ => exec(sys.exit()))
      case "GuessButton" => for
        n <- mv(nop, _ => getSpinner("number"))
        _ <- mv(guess(n), i => toLabel(i, "OutPutLabel"))
      yield ()
    })
  yield ()

  val p = pick
  println("number to guess: " + p)
  controller.run((p, initialWindow))
