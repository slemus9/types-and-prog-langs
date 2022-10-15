package common

import scala.util.control.NoStackTrace

object evaluator:

  enum EvaluationError extends NoStackTrace:
    case RuntimeError (message: String)
    case TypeError (message: String)

  type EvaluationResult [A] = Either[EvaluationError, A]

end evaluator