package arith

import cats.syntax.all._
import common.Parser.*
import common.Tokens

object parser:

  final case class Parse [F[_], A] (tokens: Tokens) (
    using t: Term[F, A]
  ):

    private val bool: Parser[F[A]] = 
      literal("true").as(t.trueConst) or
      literal("false").as(t.falseConst)

    private val zero: Parser[F[A]] = 
      literal("zero") as t.zero

    private def succ: Parser[F[A]] =
      for {
        _ <- literal("succ")
        n <- termInParen
      } yield t.succ(n)

    private def ifThenElse: Parser[F[A]] = 
      for {
        _  <- literal("if")
        t1 <- term
        _  <- literal("then")
        t2 <- term
        _  <- literal("else")
        t3 <- term
      } yield t.ifThenElse(t1, t2, t3)

    /*
      TODO: Why does using term.between(literal("("), literal(")"))
      results in stack overflow when constructing the parser?
    */
    private def termInParen: Parser[F[A]] =
      for {
        _ <- literal("(")
        t <- term
        _ <- literal(")")
      } yield t

    private def term: Parser[F[A]] =
      zero.or(succ)
          .or(bool)
          .or(ifThenElse)
          .or(termInParen)

    lazy val result = term(tokens)

  end Parse

end parser