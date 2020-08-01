package astrac.effunit
package tree
package filters

import cats.kernel.Monoid

private[filters] final class FilterAttributes private (
    val matchThis: Boolean = false,
    val matchDescendant: Boolean = false,
    val matchAncestor: Boolean = false,
    val ignoreThis: Boolean = false,
    val ignoreAncestor: Boolean = false
) {
  def matchThisOrDescendant =
    !ignoreThis && !ignoreAncestor && matchThis || matchDescendant

  def isMatched =
    !ignoreThis && !ignoreAncestor && (matchThis || matchAncestor || matchDescendant)
}

object FilterAttributes {
  val NoAttributes = new FilterAttributes()
  val MatchThis = new FilterAttributes(matchThis = true)
  val MatchAncestor = new FilterAttributes(matchAncestor = true)
  val MatchDescendant = new FilterAttributes(matchDescendant = true)
  val IgnoreThis = new FilterAttributes(ignoreThis = true)
  val IgnoreAncestor = new FilterAttributes(ignoreAncestor = true)

  implicit val monoid: Monoid[FilterAttributes] =
    Monoid.instance(
      NoAttributes,
      (a, b) =>
        new FilterAttributes(
          matchThis = a.matchThis || b.matchThis,
          matchDescendant = a.matchDescendant || b.matchDescendant,
          matchAncestor = a.matchAncestor || b.matchAncestor,
          ignoreThis = a.ignoreThis || b.ignoreThis,
          ignoreAncestor = a.ignoreAncestor || b.ignoreAncestor
        )
    )
}
