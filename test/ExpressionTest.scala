package edu.rice.comp311.test
import edu.rice.comp311.main._
import edu.rice.comp311.main.expressions._

import junit.framework.TestCase
import org.junit.Assert._

// Unit test for methods in Expression class
case class ExpressionTest(name: String) extends TestCase(name) {
  def testReduce: Unit = {
    val arcsinX = arcsin(evar("x"))
    assertEquals("arcsin(0.5) = 0.523599", 0.523599, arcsinX.reduce("x"->0.5),0.0001)

    val lnX = ln(evar("x"))
    assertEquals("ln(1) == 0",  0, lnX.reduce("x"->1.0),0.0001)

    val sinX = sin(evar("x"))
    assertEquals("sin(0.5pi) = 1", 1.0, sinX.reduce("x"->0.5*math.Pi),0.0001)

    val expX = exp(evar("x"))
    assertEquals("exp(0) = 1", 1.0, expX.reduce("x"->0.0), 0.0001)


    val expr1 =((evar("x")**2)+(const(2.0)*evar("x"))+const(1))
    assertEquals("The expression evaluates to 16.0 if x == 3",
      16.0, expr1.reduce("x"->3.0),0.0001)

    val expr2 =((evar("x")**2)+(const(3.0)/evar("x"))-const(1))
    assertEquals("The expression evaluates to 9.0 if x == 3",
      9.0, expr2.reduce("x"->3.0),0.0001)
  }

  def testExpand(): Unit = {
    val actual = ((evar("x")**2)+(const(2.0)*evar("x"))+const(1.0)).expand("x" -> (evar("x") + evar("y")))
    val expected = (evar("x") + evar("y")) ** 2 + const(2.0) * (evar("x") + evar("y")) + const(1.0)
    assertEquals("Expaned expression is as expected", expected, actual)
  }

  def testSimplify(): Unit = {
    val expected = evar("x") * evar("z") + evar("y") * evar("z")
    val actual = ((evar("x") + evar("y")) * evar("z")).simplify()
    assertEquals("(x + y) * z == x * z + y * z", expected, actual)
  }


  def testDerivative1: Unit = {
    val expr = arcsin(evar("x") / (const(1.0) + evar("x") ** 2.0) ** (0.5))
    val delta = 0.00000001
    val x0 = 0.5


    assertEquals("Test derivative of expression1 at 0.5",
      (expr.reduce("x"-> (x0 + delta)) - expr.reduce("x" -> x0)) / delta,
      expr.differentiate("x").reduce("x" -> x0),
      0.0001
    )
  }

  def testDerivative2: Unit = {
    val expr = ln(arcsin(exp(evar("x")) / (const(1) + exp(evar("x")) ** 2) ** 0.5))
    val delta = 0.00000001
    val x0 = 0.5
    assertEquals("Test derivative of expression2 at 0.5",
      (expr.reduce("x"->(x0+delta)) - expr.reduce("x"->x0))/delta,
      expr.differentiate("x").reduce("x"->x0),
      0.0001
    )
  }

  def testDerivative3: Unit = {
    val expr = evar("x") ** 2
    val delta = 0.000001
    val x0 = 1.0
    assertEquals("Test derivative of x^2 at 1.0",
      (expr.reduce("x"->(x0 + delta)) - expr.reduce("x"->x0)) / delta,
      expr.differentiate("x").reduce("x"->x0),
      0.0001
    )

    val x1 = 0.3
    val expr1 = ln(evar("x"))
    assertEquals("Test derivative of ln(x) at 0.3",
      (expr1.reduce("x"->(x1 + delta)) - expr1.reduce("x"->x1)) / delta,
      expr1.differentiate("x").reduce("x"->x1),
      0.0001
    )

    val expr2 = exp(evar("x"))
    assertEquals("Test derivative of exp(x) at 0.3",
      (expr2.reduce("x"->(x1 + delta)) - expr2.reduce("x"->x1)) / delta,
      expr2.differentiate("x").reduce("x"->x1),
      0.0001
    )

    val expr3 = sin(evar("x"))
    val x2 = 0.5 * math.Pi
    assertEquals("Test derivative of sin(x) at 1/2*pi",
      (expr3.reduce("x"->(x2 + delta)) - expr3.reduce("x"->x2)) / delta,
      expr3.differentiate("x").reduce("x"->x2),
      0.0001
    )

    val expr4 = arcsin(evar("x"))
    assertEquals("Test derivative of arcsin(x) at 1/2*pi",
      (expr4.reduce("x"->(x2 + delta)) - expr4.reduce("x"->x2)) / delta,
      expr4.differentiate("x").reduce("x"->x2),
      0.0001
    )
  }

  def testDerivative5: Unit = {
    val expr1 = (const(2) * sin(evar("x"))) * (const(3) * arcsin(evar("x")))
    val x1= 0.5 * math.Pi
    val delta = 0.000001
    assertEquals("Test sin * arcsin",
      (expr1.reduce("x"->(x1 + delta)) - expr1.reduce("x"->x1)) / delta,
      expr1.differentiate("x").reduce("x"->x1),
      0.0001
    )

    val expr2 = (const(2) * ln(evar("x"))) / (const(3) * exp(evar("x")))
    val x2= 0.5
    assertEquals("Test ln * exp",
      (expr2.reduce("x"->(x2 + delta)) - expr2.reduce("x"->x2)) / delta,
      expr2.differentiate("x").reduce("x"->x2),
      0.0001
    )

    val expr3 = ln(arcsin(exp(evar("x"))))
    assertEquals("Test part1",
      (expr3.reduce("x"->(x2 + delta)) - expr3.reduce("x"->x2)) / delta,
      expr3.differentiate("x").reduce("x"->x2),
      0.0001
    )

//    val expr4 = (const(1) + exp(evar("x")) ** 2) ** 0.5
    val expr4 = (exp(evar("x")) ** 2)
    assertEquals("Test part2",
      (expr4.reduce("x"->(x2 + delta)) - expr4.reduce("x"->x2)) / delta,
      expr4.differentiate("x").reduce("x"->x2),
      0.0001
    )
  }
}

