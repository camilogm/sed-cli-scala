package shapes

import scala.util.matching.Regex

case class SubstitutionExpression(regex: Regex, substitution: String, containsP: Boolean, containsG: Boolean, index: Int)
