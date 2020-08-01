package astrac.effunit
package tree
package description

import cats.data.State

case class LabellerState(testNames: Set[String], level: Int)

object LabellerState {
  val empty = LabellerState(Set.empty, 0)

  def downOneLevel: State[LabellerState, Unit] =
    State.modify(s => s.copy(level = s.level + 1))

  def uniqueTestName(name: String): State[LabellerState, String] =
    State { s =>
      val escapedName = name.replace("\n", "\\n")
      val indentedName = ("| " * s.level) + escapedName
      val testName = munit.internal.Compat.LazyList
        .from(0)
        .map {
          case 0 => indentedName
          case n => s"${indentedName}-${n}"
        }
        .find(candidate => !s.testNames.contains(candidate))
        .head

      (s.copy(testNames = s.testNames + testName), testName)
    }
}

