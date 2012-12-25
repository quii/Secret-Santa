package com.quii.secretsanta

import org.scalatra._
import scalate.ScalateSupport
import scala.util.Random

class MyServlet extends ScalatraServlet with ScalateSupport {

  get("/") {
    contentType = "text/html"
    ssp("/index")
  }

  post("/"){
    contentType="text/html"
    val pairs = calculateIt(params.get("names").getOrElse("").split(' ').toSet)
    ssp("/results", "pairs" -> pairs)
  }



  def calculateIt(people: Set[String]): Map[String, String] = {

    val pairs = scala.collection.mutable.Map[String, String]()

    people foreach { person =>

      def getPersonToGiveTo(candidates: Set[String]): Option[String] = {

        def notBeingGivenYet(p: String): Boolean = !pairs.values.exists(_==p)

        def notGivingToMe(p: String): Boolean = pairs.get(p) match{
          case Some(receiver) if receiver==person => false
          case _ => true
        }

        Random.shuffle(candidates filter notBeingGivenYet filter notGivingToMe).headOption
      }

      val peopleNotMe = people.filterNot(_ == person)

      getPersonToGiveTo(peopleNotMe) match{
        case Some(luckyPerson) => pairs += person -> luckyPerson
        case None => {}
      }

    }

    pairs.toMap
  }


}
