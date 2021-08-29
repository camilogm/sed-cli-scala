package helpers

import shapes.{EvaluatedLine, SubstitutionExpression}

import scala.annotation.tailrec

object SedCases {

  private def substitution(line: String, expressions: Array[SubstitutionExpression], substitution: String): Array[EvaluatedLine] = {
    expressions.foldLeft(Array(EvaluatedLine(false, "", -1))) {
      (a: Array[EvaluatedLine], b) => {
        val containsG = b.containsG
        val containsP = b.containsP
        val pattern = b.regex
        val isMatching = !pattern.findFirstIn(line).isEmpty
        val finalString: String = {
          if (containsG && isMatching) line.replaceAll(pattern.toString(), substitution)
          else if (!containsG && isMatching) line.replaceFirst(pattern.toString(), substitution)
          else line
        }
        val newEvaluatedLIne = EvaluatedLine(isMatching, finalString, b.index)

        if (isMatching && containsP)
          a :+ newEvaluatedLIne :+ newEvaluatedLIne
        else
          a :+ newEvaluatedLIne
      }.filter((result) => result.indexExpression != -1)
    }
  }

  def linesSubstitution(linesIterator: Iterator[String], expressions: Array[SubstitutionExpression]): Either[RuntimeException, Iterator[Array[EvaluatedLine]]] = {
    @tailrec
    def applyExpression(linesIterator: Iterator[String], expressions: Array[SubstitutionExpression], line: String, resultIterator: Iterator[Array[EvaluatedLine]]): Iterator[Array[EvaluatedLine]] = {
      if (linesIterator.isEmpty) resultIterator
      else {
        val resultLine = substitution(line, expressions, "Camilo")
        applyExpression(linesIterator, expressions, linesIterator.next(), resultIterator ++ Iterator(resultLine))
      }
    }

    Right(applyExpression(linesIterator, expressions, linesIterator.next(), Iterator()))
  }
}
