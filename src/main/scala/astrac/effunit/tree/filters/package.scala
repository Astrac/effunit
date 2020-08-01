package astrac.effunit
package tree

import higherkindness.droste.data.prelude._
import munit.Only
import org.junit.runner.manipulation.Filter

import description.ChildlessDescription

package object filters {

  def only[F[_]]: UnlabelledFilter[F] =
    UnlabelledFilter.matchIf[F](_.options.tags.contains(Only))

  def junitFilter[F[_]](
      filter: Filter
  ): LabelledFilter[F, ChildlessDescription] =
    LabelledFilter.matchIf(t => filter.shouldRun(t.ask.unwrap))
}

