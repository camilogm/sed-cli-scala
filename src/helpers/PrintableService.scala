package helpers

import shapes.{EvaluatedLine, PrintableTypes}

object PrintableService {
  private def printInConsole(dataIterator: Iterator[Array[EvaluatedLine]]): Unit = {
    dataIterator.foreach(x => x.foreach(y => println(y.value)))
  }

  def printResult(printableTypes: PrintableTypes.Value, dataIterator: Iterator[Array[EvaluatedLine]]): Either[RuntimeException, Boolean] = {
    printableTypes match {
      case (PrintableTypes.Console) => {
        this.printInConsole(dataIterator)
        Right(true)
      }
      case _ => {
        Left(new RuntimeException(s"Not supported ${printableTypes}"))
      }
    }
  }
}
