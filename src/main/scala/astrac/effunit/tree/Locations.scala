package astrac.effunit
package tree

import higherkindness.droste.Algebra
import higherkindness.droste.scheme
import munit.Location

object Locations {

  def testLocations[F[_]](t: TestTree.Fixed[F]): List[Location] =
    scheme.cata(locationsAlgebra[F]).apply(t)

  private def locationsAlgebra[F[_]]: Algebra[TestTree[F, *], List[Location]] =
    Algebra {
      case Test(o, _)                   => List(o.location)
      case SharingResource(o, _, _, ts) => o.location :: ts.flatten
    }
}
