package astrac.effunit
package tree

import description.DescribedTree

package object transforms {
  type Transform[F[_]] = DescribedTree[F] => DescribedTree[F]
}
