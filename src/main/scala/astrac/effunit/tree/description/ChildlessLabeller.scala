package astrac.effunit
package tree
package description

import cats.data.State
import cats.syntax.flatMap._
import higherkindness.droste.{CoalgebraM, scheme}
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._
import org.junit.runner.Description

case class ChildlessDescription(unwrap: Description)

class ChildlessLabeller[F[_]](cls: Class[_ <: EffectfulSuite])
    extends (TestTree.Fixed[F] => LabelledTree[F, ChildlessDescription]) {

  private def description(
      t: TestTree[F, _]
  ): State[LabellerState, ChildlessDescription] = {
    LabellerState
      .uniqueTestName(t.options.name)
      .map(n =>
        ChildlessDescription(
          Description.createTestDescription(cls, n, t.annotations: _*)
        )
      )
  }

  val labellingCoalgebra: CoalgebraM[
    State[LabellerState, *],
    ChildlessTreeF[F, *],
    TestTree.Fixed[F]
  ] =
    CoalgebraM { tree =>
      TestTree.unfix(tree) match {
        case t @ Test(_, _) =>
          description(t).map(AttrF(_, t))
        case t @ SharingResource(o, e, r, ts) =>
          description(t)
            .flatTap(_ => LabellerState.downOneLevel)
            .map(AttrF(_, SharingResource(o, e, r, ts)))
      }
    }

  def apply(t: TestTree.Fixed[F]): ChildlessTree[F] =
    scheme.anaM(labellingCoalgebra).apply(t).runA(LabellerState.empty).value
}

object ChildlessLabeller {
  def apply[F[_]](cls: Class[_ <: EffectfulSuite]): ChildlessLabeller[F] =
    new ChildlessLabeller[F](cls)
}
