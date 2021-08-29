package exceptions

class InvalidCommandException(expressionsStr:String)
  extends RuntimeException(s"Invalid command format ${expressionsStr}"){
}
