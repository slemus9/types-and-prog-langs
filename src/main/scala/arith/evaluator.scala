package arith

import cats.syntax.all._
import cats.MonadError
import cats.effect.{IO, IOApp}
import values.*
import common.evaluator.*

object evaluator:

  def simpleStrictEvaluator [F[_]] (
    using F: MonadError[F, EvaluationError]
  ) = new Term[F, Value] {

    import Nat.*
    import EvaluationError.*

    def guardNat (t: F[Value]): F[Nat] = t.flatMap {
      case n: Nat => n.pure
      case _      => F.raiseError(TypeError(s"$t is not of type Nat"))
    } 

    def guardBool (t: F[Value]): F[Boolean] = t.flatMap {
      case b: Boolean => b.pure
      case _          => F.raiseError(TypeError(s"$t is not of type Boolean"))
    } 

    def isZero (t: F[Value]): F[Value] = t.map { 
      case Zero => true
      case _    => false
    }

    def succ (t: F[Value]): F[Value] = 
      guardNat(t).map(Succ(_))

    def zero: F[Value] = Zero.pure

    def pred (t: F[Value]): F[Value] = 
      guardNat(t).flatMap {
        case Zero    => F.raiseError(RuntimeError(s"zero has no predecessor"))
        case Succ(n) => n.pure
      }

    def falseConst: F[Value] = false.pure

    def ifThenElse (
      cond: F[Value], 
      ifTrue: F[Value], 
      ifFalse: F[Value]
    ): F[Value] = guardBool(cond).flatMap {
      if _ then ifTrue else ifFalse
    }

    def trueConst: F[Value] = true.pure
  }

end evaluator

object EvaluatorExample extends IOApp.Simple {

  def run: IO[Unit] = {
    given Term[EvaluationResult, Value] = evaluator.simpleStrictEvaluator

    for {
      r1 <- IO.fromEither(ev1)
      _  <- IO.println(r1)
      r2 <- IO.fromEither(ev2)
      _  <- IO.println(r2)
      r3 <- IO.fromEither(ev3).attempt
      _  <- IO.println(r3)
      r4 <- IO.fromEither(ev4)
      _  <- IO.println(r4)
    } yield ()
  }

  def ev1 (
    using interpreter: Term[EvaluationResult, Value]
  ): EvaluationResult[Value] = {
    import interpreter.*

    ifThenElse(
      falseConst,
      succ(succ(zero)),
      succ(succ(succ(zero)))
    )
  }

  def ev2 (
    using interpreter: Term[EvaluationResult, Value]
  ): EvaluationResult[Value] = {
    import interpreter.*

    ifThenElse(
      trueConst,
      ifThenElse(
        falseConst,
        succ(zero),
        zero
      ),
      zero
    )
  }

  def ev3 (
    using interpreter: Term[EvaluationResult, Value]
  ): EvaluationResult[Value] = {
    import interpreter.*

    ifThenElse(
      succ(zero),
      trueConst,
      falseConst
    )
  }

  def ev4 (
    using interpreter: Term[EvaluationResult, Value]
  ): EvaluationResult[Value] = {
    import interpreter.*

    ifThenElse(
      trueConst,
      succ(zero),
      ifThenElse(succ(zero), zero, zero)
    )
  }
}