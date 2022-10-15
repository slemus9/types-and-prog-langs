package arith

object values:

  enum Nat:
    case Zero
    case Succ (n: Nat)

  type Value = Boolean | Nat

  def isNumeric: Value => Boolean = {
    case Nat.Zero | Nat.Succ(_) => true
    case _ => false
  }

end values
