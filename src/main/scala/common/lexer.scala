package common

object lexer:

  private def isCharId (c: Char) =
    c.isLetterOrDigit || c == '_'

  def clex (n: Int, m: Int, chars: LazyList[Char]): LazyList[Token] = 
    chars match
      case chars if chars.isEmpty => LazyList.empty

      // New line
      case '\n' #:: chars => clex(n + 1, 1, chars)

      // Ignore whitespace
      case c #:: chars if c.isWhitespace => clex(n, m + 1, chars)

      // Ignore comments of the form: -- ... \n
      case '-' #:: '-' #:: chars => 
        val rem = chars.dropWhile(_ != '\n')
        if rem.isEmpty then 
          LazyList.empty 
        else
          clex(n + 1, 1, rem.tail)

      // Recognize numbers
      case s @ c #:: chars if c.isDigit => 
        val (numToken, rem) = s.span(_.isDigit)
        val numTokenStr = numToken.mkString
        Token(n, m, numTokenStr) #:: clex(n, m + numTokenStr.size, rem)

      // Recognize a letter followed by one or more letters, digits or underscores
      case s @ c #:: chars if c.isLetter =>
        val (varToken, rem) = s.span(isCharId)
        val varTokenStr = varToken.mkString
        Token(n, m, varTokenStr) #:: clex(n, m + varTokenStr.size, rem)

      // Single char token if none of the above rules are met
      case c #:: chars =>
        Token(n, m, s"$c") #:: clex(n, m + 1, chars)
  end clex

  def getTokens (chars: LazyList[Char]) = clex(1, 1, chars)

end lexer

object LexerExample extends App:

  val program = 
    """-- map function for lists
    |map f [] = []
    |map f (x : xs) = f x : map xs
    |-- foldr function for lists
    |foldr f z [] = z
    |foldr f z (x : xs) = f x (foldr f z xs)
    |
    |foldr_map1 f = foldr (\x next -> f x : next) []
    |-- end of program
    """.stripMargin

  println(program)

  println(
    lexer
      .clex(1, 1, LazyList.from(program))
      .toList
    )