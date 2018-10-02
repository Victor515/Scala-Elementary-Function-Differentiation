package edu.rice.comp311.main

abstract class Expression {
  def *(that: Expression): Expression = {
    Mutiply(this, that)
  }
  def **(d: Double): Expression = {
    Power(this, Constant(d))
  }
  def +(that: Expression): Expression = {
    Plus(this, that)
  }
  def -(that: Expression): Expression = {
    Minus(this, that)
  }
  def /(that: Expression): Expression = {
    Divide(this, that)
  }

  def reduce(env: (String, Double)*): Double

  def expand(env: (String, Expression)*): Expression

  def simplify(): Expression

  def differentiate(x: String): Expression

  override def toString: String = super.toString
}

case class Constant(d: Double) extends Expression

case class Var(x: String) extends Expression

case class Ln(x: Expression) extends Expression

case class Exp(x: Expression) extends Expression

case class Sin(x: Expression) extends Expression

case class ArcSin(x: Expression) extends Expression

case class Plus(first: Expression, second: Expression) extends Expression

case class Minus(first: Expression, second: Expression) extends Expression

case class Mutiply(first: Expression, second: Expression) extends Expression

case class Divide(first: Expression, second: Expression) extends Expression

case class Power(base: Expression, power: Constant) extends Expression