package helpers

import exceptions.{EmptyExpressionsException, InvalidCommandException}
import shapes.{CommandExpressions, PrintableTypes, SubstitutionExpression}

import scala.util.matching.Regex

object VerificationService {
  private def checkExpressions(expressions: Array[String]): Either[RuntimeException, List[SubstitutionExpression]] = {
    val regex = new Regex("(s)\\/[a-zA-Z|\\\\]+\\/[a-zA-Z]+\\/(g|p|)")
    val validatedExpressions = expressions.filter((expression) => regex.findAllMatchIn(expression).mkString("") == expression).toList

    val invalidExpressions = expressions.filter(expression => !validatedExpressions.contains(expression))
    if (invalidExpressions.length > 0) {
      val invalidExpressionsStr = invalidExpressions.mkString(",")
      Left(new InvalidCommandException(invalidExpressionsStr))
    }
    else {


      val transformedExpressions = validatedExpressions.zipWithIndex.map { case (expression: String, index: Int) => {
        val expr = expression.split("/", 4)
        val pattern = expr(1)
        val substitution = expr(2)
        val flags = expr(3)
        shapes.SubstitutionExpression(pattern.r, substitution, flags.contains("p"), flags.contains("g"), index)
      }
      }

      Right(transformedExpressions)
    }
  }

  private def validateFlagN(expressions: List[SubstitutionExpression]): Either[RuntimeException, List[SubstitutionExpression]] = {
    val expressionsWithP = expressions.filter(_.containsP)
    if (expressionsWithP.length > 0) Right(expressionsWithP)
    else Left(new EmptyExpressionsException)
  }


  def extractCommands(args: Array[String]): Either[RuntimeException, CommandExpressions] = {
    val flagN = args.contains("-n")
    val flagI = args.contains("-i")

    if (flagN && flagI) Left(new RuntimeException("The command cannot contain flag i and n at the same time"))
    else {
      val printable = if (flagI) PrintableTypes.OverrideFile else PrintableTypes.Console

      val commandArgs = args(0)
      val filePath = args(1)
      val result = for {
        expressions <- this.checkExpressions(Array(commandArgs))
        validatedN <- if (flagN) this.validateFlagN(expressions) else Right(expressions)
        commandExpression <- Right(CommandExpressions(printable, flagN, filePath, validatedN))
      } yield commandExpression

      result
    }
  }
}
