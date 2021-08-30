package helpers

import shapes.{EvaluatedLine, SubstitutionExpression}

import scala.annotation.tailrec

object SedCases {

  private def substitution(line: String, expressions: List[SubstitutionExpression], substitution: String, flagN:Boolean): List[EvaluatedLine] = {
    expressions.foldLeft(List(EvaluatedLine(false, "", -1,false))) {
      (a: List[EvaluatedLine], b) => {
        val containsG = b.containsG
        val containsP = b.containsP
        val pattern = b.regex
        val isMatching = !pattern.findFirstIn(line).isEmpty
        val finalString: String = {
          if (containsG && isMatching) line.replaceAll(pattern.toString(), substitution)
          else if (!containsG && isMatching) line.replaceFirst(pattern.toString(), substitution)
          else line
        }
        val toBePrinted = !(flagN &&  containsP)
        val newEvaluatedLine = EvaluatedLine(isMatching, finalString, b.index, toBePrinted)

        if (isMatching && containsP)
          a :+ newEvaluatedLine :+ EvaluatedLine(isMatching,finalString,b.index,!toBePrinted)
        else
          a :+ newEvaluatedLine
      }.filter((result) => result.indexExpression != -1)
    }
  }

  def linesSubstitution(linesIterator: Iterator[String], expressions: List[SubstitutionExpression],flagN : Boolean): Either[RuntimeException, Iterator[List[EvaluatedLine]]] = {
    @tailrec
    def applyExpression(linesIterator: Iterator[String], expressions: List[SubstitutionExpression], line: String, resultIterator: Iterator[List[EvaluatedLine]]): Iterator[List[EvaluatedLine]] = {
      if (linesIterator.isEmpty) resultIterator
      else {
        val resultLine = substitution(line, expressions, "Camilo",flagN)
        applyExpression(linesIterator, expressions, linesIterator.next(), resultIterator ++ Iterator(resultLine))
      }
    }

    Right(applyExpression(linesIterator, expressions, linesIterator.next(), Iterator()))
  }
}
