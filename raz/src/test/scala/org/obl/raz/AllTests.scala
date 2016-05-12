package org.obl.raz

import org.scalatest.Suites
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalatest.FunSuite
import org.scalatest.Ignore

/*
class AllTests extends Suites(
  new CodecTest,
  new ConverterTest,
  new DecoderTest,
  new EncoderTest,
  new PathBuilderTest,
  new PathBuildersTest,
  new UriTemplateEncoderTest,
  new ScalacheckSuite(PathGen)(Gen.Parameters.default))
*/

class ScalacheckSuite(props: Properties*)(implicit pars: Gen.Parameters) extends FunSuite {

  props.foreach { prop =>
    prop.properties.foreach { (t) =>

      val (desc, property) = t
      val label = prop.name + " " + desc
      
      test(label) {

        val res = property(pars)
        val success = res.success

        assert(success, label)
      }

    }
  }

}

/*
class AllChecks extends FunSuite {

  test(PathGen.name) {
    val r = PathGen.apply(Gen.Parameters.default)

    PathGen.properties.foreach { (t) =>
      val (desc, prop) = t
      assert(r.success, r.status)
    }

  }

}
*/