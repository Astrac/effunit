package astrac.effunit
package utils

import munit.{Location, TestOptions}

import mutable._

case class MetaBuilder[F[_]](
    builder: MutableBuilder[F],
    location: Location
) {}

object MetaBuilder {
  type ExpectedMetaFunction = (TestOptions, Int) => TestMeta

  def expectedMeta(location: Location): ExpectedMetaFunction =
    (options, relLines) =>
      TestMeta(
        options.name,
        options.tags,
        new Location(location.path, location.line + relLines)
      )

  def apply[F[_]](implicit
      loc: Location
  ): (MutableBuilder[F], ExpectedMetaFunction) =
    (MutableBuilder[F], expectedMeta(loc))
}

