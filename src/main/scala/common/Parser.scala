package common

import cats.syntax.all._
import cats.{Functor, Monad}
import util.{Try, Failure, Success}
import scala.annotation.tailrec

object Parser:

  final case class ParsingError (
    token: Token,
    message: String
  ):
    override def toString (): String = 
      val n = token.lineNumber
      val m = token.columnNumber
      s"For token: ${token.value}:\n\t Parsing error at line $n, column $m: $message"
  end ParsingError

  final case class ParsingSuccess [A] (
    parsed: A,
    remaining: Tokens
  )

  type ParsingResult [A] = Either[ParsingError, ParsingSuccess[A]]

  object ParsingResult:
    def error [A] (token: Token, message: String): ParsingResult[A] = 
      ParsingError(token, message).asLeft

    def success [A] (parsed: A, remaining: Tokens): ParsingResult[A] =
      ParsingSuccess(parsed, remaining).asRight

  end ParsingResult

  opaque type Parser [A] = Tokens => ParsingResult[A]

  // Instances
  given functor: Functor[Parser] = new Functor[Parser]:
    def map[A, B] (p: Parser[A]) (f: A => B): Parser[B] = 
      p(_).map { res => 
        res.copy(parsed = f(res.parsed))
      }

  given monad: Monad[Parser] with

    def flatMap [A, B] (p: Parser[A]) (f: A => Parser[B]): Parser[B] = 
      p(_).flatMap { res => 
        f (res.parsed) (res.remaining)
      }

    def tailRecM [A, B] (a: A) (f: A => Parser[Either[A, B]]): Parser[B] = {
      
      @tailrec
      def go (acc: ParsingResult[Either[A, B]]): ParsingResult[B] = acc match
        case Right(ParsingSuccess(Left(a), rem))  => go(f (a) (rem))
        case Right(ParsingSuccess(Right(b), rem)) => ParsingSuccess(b, rem).asRight
        case Left(e)                              => e.asLeft
      
      f(a) andThen go
    }

    def pure[A](x: A): Parser[A] = ParsingSuccess(x, _).asRight
  

  // Combinators
  def success [A] (a: A): Parser[A] = a.pure

  def failure [A] (error: ParsingError): Parser[A] = _ => error.asLeft

  def single: Parser[Token] = _ match
    case ts if ts.isEmpty => ParsingResult.error(Token.empty, "Expected one token")
    case t #:: ts         => ParsingResult.success(t, ts)

  def trySingle [A] (unsafe: Token => A): Parser[A] = single.flatMap { t => 
    Try(unsafe(t)) match
      case Failure(e) => failure(ParsingError(t, e.toString))
      case Success(a) => success(a) 
  }
  
  def satisfies (
    pred: Token => Boolean, onError: String
  ): Parser[String] = single.flatMap { t => 
    if pred(t) then success(t.value) 
    else failure(ParsingError(t, onError))
  }

  def literal (s: String): Parser[String] = satisfies(
    _.value == s, 
    s"Expected value: $s"
  )

  def belongsTo (values: Set[String]): Parser[String] = satisfies(
    t => values.contains(t.value), 
    s"Expected one of the following values: ${values}"
  )

  def string: Parser[String] = single.map(_.value)

  def boolean: Parser[Boolean] = trySingle(_.value.toBoolean)

  def int: Parser[Int] = trySingle(_.value.toInt)

  extension [A] (p1: Parser[A])

    def apply (tokens: Tokens) = p1(tokens)

    /*
      If the `p1` parser fails, fallback to parsing with `p2`
    */
    def or (p2: Parser[A]): Parser[A] = 
      inp => p1(inp) match
        case r @ Right(_) => r
        case Left(_)      => p2(inp)
      

    def zeroOrMore: Parser[LazyList[A]] = p1.oneOrMore.or(LazyList.empty.pure)

    def oneOrMore: Parser[LazyList[A]] =
      for {
        a   <- p1
        rem <- p1.zeroOrMore
      } yield a #:: rem

    /*
      Recognizes one or more occurences of a symbol (first parser),
      separated by another symbol (second parser)
    */
    def oneOrMoreWithSep [B] (p2: Parser[B]): Parser[LazyList[A]] = 
      for {
        a   <- p1
        rem <- (p2 >> p1).zeroOrMore
      } yield a #:: rem

    def between [B, C] (p2: Parser[B], p3: Parser[C]): Parser[A] =
      for {
        _ <- p2
        a <- p1
        _ <- p3
      } yield a

end Parser

object ParserExample extends App {

  import Parser._

  val s = "xs_1 = [1, 2, 3, 4]"
  val tokens = lexer.getTokens(LazyList.from(s))
  val parser = for {
    name <- literal("xs_1")
    _    <- literal("=")
    list <- int.oneOrMoreWithSep(literal(",")).between(
      literal("["), literal("]")
    )
  } yield (name, list.toList)

  val parsed = parser(tokens)

  println(tokens)
  println(parsed)
}