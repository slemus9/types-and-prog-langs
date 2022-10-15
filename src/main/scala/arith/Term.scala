package arith

trait Term [F[_], A]:
  def trueConst: F[A]

  def falseConst: F[A]

  def zero: F[A]

  def succ (t: F[A]): F[A]

  def pred (t: F[A]): F[A]

  def isZero (t: F[A]): F[A]

  def ifThenElse (cond: F[A], ifTrue: F[A], ifFalse: F[A]): F[A]
end Term