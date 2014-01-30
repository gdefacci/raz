package org.obl.raz.plans

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ GivenWhenThen, FeatureSpec }
import unfiltered.scalatest.jetty.Served
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import unfiltered.scalatest.Hosted
import dispatch.classic.StatusCode

trait FunsuiteServed extends FunSuite with Hosted {
  import unfiltered.jetty._
  def setup: (Server => Server)

  override protected def withFixture(test: NoArgTest) {
    lazy val server = setup(Http(port))
    server.start();
    try {
      test() // Invoke the test function
    } finally {
      server.stop();
      server.destroy();
    }
  }

  def assertCode[T](i: Int)(v: => T) = {
    try {
      v
      fail("no exception raised")
    } catch {
      case e: StatusCode if e.code == i => ()
    }
  }

}