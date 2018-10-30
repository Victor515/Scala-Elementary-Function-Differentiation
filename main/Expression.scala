package edu.rice.comp311.main

/**
  * Abstract implementation of elementary functions and their operations
  */
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

  // override on abstract methods does not require override keyword
  def reduce(env: (String, Double)*): Double

  def expand(env: (String, Expression)*): Expression

  def simplify(): Expression

  def differentiate(x: String): Expression

  override def toString: String = super.toString
}

/**
  * Class definition for constants
  */
case class Constant(d: Double) extends Expression {
  override def reduce(env: (String, Double)*): Double = {
    d
  }

  override def expand(env: (String, Expression)*): Expression = {
    this
  }

  override def simplify(): Expression = {
    this
  }

  override def differentiate(x: String): Expression = {
    Constant(0)
  }
}

/**
  * Class definition for variables
  */
case class Var(x: String) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    env.find((pair) => pair._1 == x).get._2
  }

  override def expand(env: (String, Expression)*): Expression = {
    env.find((pair) => pair._1 == x).get._2
  }

  override def simplify(): Expression = {
    this
  }

  override def differentiate(x: String): Expression = {
    if(this.x == x){
      Constant(1)
    }else{
      Constant(0)
    }
  }
}

/**
  * Class definition for natural log expressions
  */
case class Ln(x: Expression) extends Expression {
  override def reduce(env: (String, Double)*): Double = {
    math.log(x.reduce(env:_*))
  }

  override def expand(env: (String, Expression)*): Expression = {
    Ln(x.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      // an immediate application of ln(x) to e^F simplifies to F
      case Ln(Exp(x)) => x
      case Ln(Constant(c)) => Constant(math.log(c))
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    (Constant(1) / this.x) * this.x.differentiate(x)
  }
}

/**
  * Class definition for exponential expressions
  */
case class Exp(x: Expression) extends Expression {
  override def reduce(env: (String, Double)*): Double = {
    math.exp(x.reduce(env:_*))
  }

  override def expand(env: (String, Expression)*): Expression = {
    Exp(x.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      // an immediate application of e^x to ln(F) simplifies to F
      case Exp(Ln(x)) => x
      case Exp(Constant(c)) => Constant(math.exp(c))
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    Exp(this.x) * this.x.differentiate(x)
  }
}

/**
  * Class definition for sin expressions
  */
case class Sin(x: Expression) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    math.sin(x.reduce(env:_*))
  }

  override def expand(env: (String, Expression)*): Expression = {
    Sin(x.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      case Sin(Constant(c)) => Constant(math.sin(c))
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    Sin(this.x + Constant(0.5 * math.Pi)) * this.x.differentiate(x)
  }
}

/**
  * Class definition for arcsin expressions
  */
case class ArcSin(x: Expression) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    math.asin(x.reduce(env:_*))
  }

  override def expand(env: (String, Expression)*): Expression = {
    ArcSin(x.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      case ArcSin(Constant(c)) => Constant(math.asin(c))
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    // 1 / (1-x^2)^(1/2)
    (Constant(1) / ((Constant(1) - this.x**2)**(0.5))) * this.x.differentiate(x)
  }
}

/**
  * Class definition for plus operators
  */
case class Plus(first: Expression, second: Expression) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    first.reduce(env:_*) + second.reduce(env:_*)
  }

  override def expand(env: (String, Expression)*): Expression = {
    Plus(first.expand(env:_*), second.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      case Plus(Constant(0), expression) => expression // need to simplify further
      case Plus(expression, Constant(0)) => expression // need to simplify further
      case Plus(Constant(c1), Constant(c2)) => Constant(c1 + c2)
      case expression => expression // need to simplify further
    }
  }

  override def differentiate(x: String): Expression = {
    first.differentiate(x) + second.differentiate(x)
  }
}

/**
  * Class definition for minus operators
  */
case class Minus(first: Expression, second: Expression) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    first.reduce(env:_*) - second.reduce(env:_*)
  }

  override def expand(env: (String, Expression)*): Expression = {
    Minus(first.expand(env:_*), second.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match{
      case Minus(expression, Constant(0)) => expression
      case Minus(Constant(c1), Constant(c2)) => Constant(c1 - c2)
      case expression => expression
    }
  }

  override def differentiate(x: String): Expression = {
    first.differentiate(x) - second.differentiate(x)
  }
}

/**
  * Class definition for mutiply operators
  */
case class Mutiply(first: Expression, second: Expression) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    first.reduce(env:_*) * second.reduce(env:_*)
  }

  override def expand(env: (String, Expression)*): Expression = {
    Mutiply(first.expand(env:_*), second.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      case Mutiply(Constant(0), _) => Constant(0)
      case Mutiply(_, Constant(0)) => Constant(0)
      case Mutiply(Constant(1), expression) => expression // need to simplify further
      case Mutiply(expression, Constant(1)) => expression // need to simplify further
      case Mutiply(Power(base1, Constant(x)), Power(base2, Constant(y))) => {
        if(base1 == base2){
          base1 ** (x + y)
        }else{
          this
        }
      }
      case Mutiply(Plus(expr1, expr2), Plus(expr3, expr4)) =>
        (expr1 * expr3) + (expr1 * expr4) + (expr2 * expr3) + (expr2 * expr4)
      case Mutiply(Plus(expr1, expr2), expr3) => (expr1 * expr3) + (expr2 * expr3)
      case Mutiply(Constant(c1), Constant(c2)) => Constant(c1 * c2)
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    first.differentiate(x) * second + second.differentiate(x) * first
  }
}

/**
  * Class definition for divide operators
  */
case class Divide(first: Expression, second: Expression) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    first.reduce(env:_*) / second.reduce(env:_*)
  }

  override def expand(env: (String, Expression)*): Expression = {
    Divide(first.expand(env:_*), second.expand(env:_*))
  }

  override def simplify(): Expression = {
    this match {
      case Divide(Constant(0), _) => Constant(0)
      case Divide(Constant(c1), Constant(c2)) => Constant(c1/c2)
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    (first.differentiate(x) * second - first * second.differentiate(x)) / (second ** 2)
  }
}

/**
  * Class definition for power operators
  */
case class Power(base: Expression, power: Constant) extends Expression{
  override def reduce(env: (String, Double)*): Double = {
    math.pow(base.reduce(env:_*), power.reduce(env:_*))
  }

  override def expand(env: (String, Expression)*): Expression = {
    Power(base.expand(env:_*), power)
  }

  override def simplify(): Expression = {
    this match {
      case Power(_, Constant(0)) => Constant(1)
      case Power(base, Constant(1)) => base
      case Power(Constant(c1), Constant(c2)) => Constant(math.pow(c1, c2))
      case expr => expr
    }
  }

  override def differentiate(x: String): Expression = {
    power * Power(base, Constant(power.d - 1)) * base.differentiate(x)
  }
}