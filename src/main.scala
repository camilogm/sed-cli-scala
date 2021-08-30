import exceptions.EmptyExpressionsException
import helpers.{PrintableService, ReadFileService, SedCases, VerificationService}

object main extends App{

  val result = for {
    validCommand <- VerificationService.extractCommands(args)
    readFile <- ReadFileService.readFile(validCommand.filePath)
    sedApply <- SedCases.linesSubstitution(readFile,validCommand.expressions,validCommand.flagN)
    printable <- PrintableService.printResult(validCommand,sedApply)
  } yield printable

  result match {
    case Left(error) => {
      error match {
        case (error: EmptyExpressionsException) => println()
        case (error:RuntimeException) => println(error.getMessage)
      }
    }
    case Right(_) => println()
  }
}
