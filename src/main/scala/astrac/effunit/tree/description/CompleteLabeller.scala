package astrac.effunit
package tree
package description

import higherkindness.droste.{Trans, scheme}
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._
import org.junit.runner.Description

case class CompleteDescription(unwrap: Description)

class CompleteLabeller[F[_]] extends (ChildlessTree[F] => DescribedTree[F]) {

  val labellingTrans
      : Trans[ChildlessTreeF[F, *], DescribedTreeF[F, *], DescribedTree[F]] =
    Trans {
      case AttrF(desc, t @ Test(_, _)) =>
        AttrF(CompleteDescription(desc.unwrap), t)
      case AttrF(desc, t @ SharingResource(_, _, _, ts)) =>
        val unwrapped = desc.unwrap
        ts.map(_.head.unwrap).foreach(unwrapped.addChild)
        AttrF(CompleteDescription(unwrapped), t)
    }

  def apply(t: ChildlessTree[F]): DescribedTree[F] =
    scheme.cata(labellingTrans.algebra).apply(t)
}

object CompleteLabeller {
  def apply[F[_]]: CompleteLabeller[F] = new CompleteLabeller[F]
}
