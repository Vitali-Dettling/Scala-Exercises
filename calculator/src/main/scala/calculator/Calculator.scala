package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  //Example: Map("a" -> Signal(Plus(Literal(2), Literal(2)))) => a -> 2+2
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =  {
    namedExpressions.map(p => (p._1 -> Signal(eval(p._2(), namedExpressions))))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match{
      case p: Plus => (eval(p.a, references) + eval(p.b, references))
      case m: Minus => (eval(m.a, references) - eval(m.b, references))
      case t: Times => (eval(t.a, references) * eval(t.b, references))
      case d: Divide => (eval(d.a, references) / eval(d.b, references))
      case r: Ref =>  if(checkCycle(r.name, references)) Double.NaN
                      else eval(getReferenceExpr(r.name, references), references)
      case l: Literal => l.v
      case _ => Double.NaN//Error handling!
    }
  }

  var pre: Map[String, Expr] = Map()//Global Variable
  def checkCycle(refName: String, references: Map[String, Signal[Expr]]): Boolean = {
    if(pre.exists(p => p._1 == refName)){ pre = Map(); true }
    else{ pre += (refName -> getReferenceExpr(refName, references)) ; false }
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
