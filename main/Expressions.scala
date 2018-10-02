package edu.rice.comp311.main

object expressions{
  def evar(s: String): Expression = Var(s)
  def const(x: Double): Expression = Constant(x)
  def sin(x: Expression): Expression = Sin(x)
  def ln(x: Expression): Expression = Ln(x)
  def exp(x: Expression): Expression = Exp(x)
  def arcsin(x: Expression): Expression = ArcSin(x)
}
