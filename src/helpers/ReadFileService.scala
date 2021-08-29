package helpers

import java.io.{File, FileNotFoundException}
import scala.io.Source

object ReadFileService {

  def readFile(path: String): Either[FileNotFoundException, Iterator[String]] = {
    val fileExists = new File(path).exists();

    if (fileExists) Right(Source.fromFile(path).getLines())
    else Left(new FileNotFoundException(s"File or path doesn't exist: ${path}"))
  }
}
