package com.quii.secretsanta

import org.scalatra.test.specs2._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class MyServletSpec extends ScalatraSpec { def is =
  "GET / on MyServlet"                     ^
    "should return status 200"                  ! root200^
                                                end

  addServlet(classOf[MyServlet], "/*")

  def root200 = get("/") {
    status must_== 200
  }
}
