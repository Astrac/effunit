package astrac.effunit
package tree

package object description {
  type ChildlessTree[F[_]] = LabelledTree[F, ChildlessDescription]
  type ChildlessTreeF[F[_], A] = LabelledTreeF[F, ChildlessDescription, A]

  type DescribedTree[F[_]] = LabelledTree[F, CompleteDescription]
  type DescribedTreeF[F[_], A] = LabelledTreeF[F, CompleteDescription, A]
}
