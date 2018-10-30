package edu.rice.comp311.main

// Collections of methods to construct a symbolic elementary functions
object expressions{
  // create a variable expression with name == s
  def evar(s: String): Expression = Var(s)

  // create a constant expression with value == x
  def const(x: Double): Expression = Constant(x)

  // create a sin expression with x as its inner expression
  def sin(x: Expression): Expression = Sin(x)

  // create a natural log expression with x as its inner expression
  def ln(x: Expression): Expression = Ln(x)

  // create a natural exponential expression with x as its inner expression
  def exp(x: Expression): Expression = Exp(x)

  // create a arcsin expression with x as its inner expression
  def arcsin(x: Expression): Expression = ArcSin(x)
}
