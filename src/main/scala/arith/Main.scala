package arith

import cats.effect.{IO, IOApp}
import common.lexer
import common.evaluator.*
import values.Value

object Main extends IOApp.Simple:

  // Example program
  // TODO: Read the program from file
  val prog1 = 
    """
    if true then 
      succ(
        if false then zero else succ(succ(zero))
      )
    else 
      false
    """

  def exec (s: LazyList[Char]) (using interpreter: Term[EvaluationResult, Value]) = 
    val tokens = lexer.getTokens(s)
    parser.Parse(tokens).result

  def run: IO[Unit] = 
    given Term[EvaluationResult, Value] = evaluator.simpleStrictEvaluator
    IO(exec(
      LazyList.from(prog1)
    )).flatMap(IO.println)

end Main