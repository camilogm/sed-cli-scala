package shapes

case class EvaluatedLine(matchValue: Boolean, value: String, indexExpression: Int, toBePrint:Boolean){
  override def toString: String = this.value
}
