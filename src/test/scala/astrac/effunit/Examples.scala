package astrac.effunit

import munit.{Tag, TestOptionsConversions}
import cats.effect.{IO, Resource}

import utils._

object Examples extends TestOptionsConversions {
  val shouldNeverRun = new RuntimeException("This was not expected to be run")
  val aTag = new Tag("a-tag")

  val unitResource = Resource.liftF(IO.raiseError[Unit](shouldNeverRun))

  val notExpectedToRun = {
    val metaBuilder @ (b, _) = MetaBuilder[IO]
    b.sharingResource("res1", unitResource) { _ =>
      b.test("res1.test1") { throw shouldNeverRun }
      b.testPure("res1.test2".fail) { throw shouldNeverRun }
      b.sharingResource("res1.res2", unitResource) { _ =>
        b.test("res1.res2.test1".ignore) { throw shouldNeverRun }
        b.test("res1.res2.test2".tag(aTag)) { throw shouldNeverRun }
      }
    }
    b.test("root-test-1") { throw shouldNeverRun }
    b.testPure("root-test-2") { throw shouldNeverRun }
    metaBuilder
  }

  def tagged(t: Tag) = {
    val metaBuilder @ (builder, _) = MetaBuilder[IO]
    builder.sharingResource("untagged-root-res", unitResource) { _ =>
      builder.testPure("untagged-root-res-test") {}
      builder.testPure("tagged-root-res-test".tag(t)) {}
      builder.sharingResource("tagged-sub-res".tag(t), unitResource) { _ =>
        builder.testPure("untagged-sub-res-test") {}
        builder.sharingResource("untagged-sub-sub-res", unitResource) { _ =>
          builder.testPure("untagged-sub-sub-res-test") {}
        }
      }
    }
    builder.testPure("untagged-root-test") {}
    builder.testPure("tagged-root-test".tag(t)) {}
    metaBuilder
  }
}

