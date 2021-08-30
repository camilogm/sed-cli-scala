package helpers

import shapes.{CommandExpressions, EvaluatedLine, PrintableTypes}

object PrintableService {
  private def printInConsole(dataIterator: Iterator[List[EvaluatedLine]], flagN: Boolean): Unit = {
    if(flagN) dataIterator.foreach(x => x.filter(y => y.toBePrint).foreach(println))
    else dataIterator.foreach(x => x.foreach(y => println(y.value)))
  }

  def printResult(command: CommandExpressions, dataIterator: Iterator[List[EvaluatedLine]]): Either[RuntimeException, Boolean] = {
    val printableTypes = command.flagPrintable
    val flagN = command.flagN

    printableTypes match {
      case (PrintableTypes.Console) => {
        this.printInConsole(dataIterator, flagN )
        Right(true)
      }
      case _ => {
        Left(new RuntimeException(s"Not supported ${printableTypes}"))
      }
    }
  }
}
