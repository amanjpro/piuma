//package tests
//
//
//import org.scalatest.FunSuite
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//import ch.usi.inf.l3.piuma.transform.dsl.ParseTransformerDSL
//
//@RunWith(classOf[JUnitRunner])
//class Test extends FunSuite {
//  import ParseTransformerDSL._
//  test("should not parse") {
//    val text = """|name := "plugin"
//      |runsAfter := List("another", "no")
//      |runsRightAfter := "previous"
//      |runsBefore := List("Next")
//      |transform = {
//      |tree match {
//      |{
//      |val d = 3
//      |%>
//      |}
//      |}
//      |}""".stripMargin
//    assert(parse(text) === false)
//  }
//  
//  test("should parse") {
//    val text = """|name := "plugin"
//      |runsAfter := List("another", "no")
//      |runsBefore := List("Next")
//      |transform = {
//      |<--%
//      |val b = 3
//      |%-->
//      |tree match <--%
//      |val d = 3
//      |%-->
//      |}""".stripMargin
//    assert(parse(text) === true)
//  }
//}
