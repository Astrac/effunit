package astrac.effunit

import higherkindness.droste.data.{Attr, AttrF}

package object tree {

  type LabelledTreeF[F[_], L, A] = AttrF[TestTree[F, *], L, A]
  type LabelledTree[F[_], L] = Attr[TestTree[F, *], L]

}
