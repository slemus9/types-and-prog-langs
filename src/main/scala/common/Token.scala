package common

final case class Token (
  lineNumber: Int,
  columnNumber: Int,
  value: String
)

object Token:
  def empty = Token(-1, -1, "")

type Tokens = LazyList[Token]