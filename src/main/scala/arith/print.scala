package arith

import cats.syntax.all._
import cats.{Applicative, Id}

object print:

  final case class SimplePrintState (
    printed: String,
    isRecursive: Boolean
  )

  private def simpleFunctionPrint [F[_]: Applicative] (
    name: String,
    args: List[F[SimplePrintState]]
  ): F[SimplePrintState] = args.sequence.map { terms => 
    val withParen = terms.map { st => 
      if st.isRecursive then s"(${st.printed})" else st.printed
    }
    
    SimplePrintState(
      (name :: withParen).mkString(" "),
      true
    )
  }

  def simplePrintInterpreter [F[_]: Applicative] = new Term [F, SimplePrintState]:
    def falseConst: F[SimplePrintState] = SimplePrintState("false", false).pure

    def trueConst: F[SimplePrintState] = SimplePrintState("true", false).pure

    def zero: F[SimplePrintState] = SimplePrintState("zero", false).pure

    def pred (t: F[SimplePrintState]): F[SimplePrintState] = 
      simpleFunctionPrint("pred", List(t))
    
    def succ (t: F[SimplePrintState]): F[SimplePrintState] = 
      simpleFunctionPrint("succ", List(t))

    def isZero (t: F[SimplePrintState]): F[SimplePrintState] = 
      simpleFunctionPrint("isZero", List(t))


    def ifThenElse (
      cond: F[SimplePrintState], 
      ifTrue: F[SimplePrintState], 
      ifFalse: F[SimplePrintState]
    ): F[SimplePrintState] = 
      ( cond
      , ifTrue
      , ifFalse
      ).mapN { (p, t1, t2) =>  
        SimplePrintState(
          s"if ${p.printed} then\n  ${t1.printed} \nelse\n  ${t2.printed}",
          true
        )
      }
      
  end simplePrintInterpreter

end print

object PrintExample extends App:

  import print.SimplePrintState

  def program [F[_], A] (using interpreter: Term[F, A]): F[A] = {
    import interpreter._

    ifThenElse(
      falseConst,
      succ(succ(zero)),
      succ(succ(succ(zero)))
    )
  }

  given interpreter: Term[Id, SimplePrintState] = print.simplePrintInterpreter[Id]

  
  println(program[Id, SimplePrintState].printed)
