package edu.rice.comp311.main

abstract class Expression {
  def *(that: Expression): Expression
  def **(d: Double): Expression
  def +(that: Expression): Expression
  def -(that: Expression): Expression
  def /(that: Expression): Expression

  def reduce(env: (String, Double)*): Double

  def expand(env: (String, Expression)*): Expression

  def simplify(): Expression

  def differentiate(x: String): Expression

  override def toString: String = super.toString
}

case class Constant(n: Double) extends Expression

case class Var(x: String) extends Expression

case class Ln(x: Expression) extends Expression

case class Exp(x: Expression) extends Expression

case class Sin(x: Expression) extends Expression

case class ArcSin(x: Expression) extends Expression