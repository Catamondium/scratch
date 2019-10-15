/*
TODO:
  flatten or(or(X)) => or(X) and with 'And'
  A & Not(A) => False
  A | Not(A) => True
  
  And(False) => False
  Or(False) => False
  and(True) => True
  or(True) => True

  elide 'True' exprs from and/ors
*/


sealed class Expr {
    def or(exp: Expr): Expr =
    exp match {
        case Or(subs) => return Or(subs :+ this)
        case _ => return Or(List(this, exp))
    }

    def and(exp: Expr): Expr =
    exp match {
        case And(subs) => return And(subs :+ this)
        case _ => return And(List(this, exp))
    }

    def unary_!(): Expr = this match {
      case Not(exp) => return exp // doubled, unwrap
      case And(exps) => return Or(exps.map(!(_))) // DeMorgan
      case Or(exps) => return And(exps.map(!(_))) // DeMorgan
      case Sym(_) => return Not(this) // wrap
    }
    
    // Supplemental operators
    def +(exp: Expr) = or(exp)
    def |(exp: Expr) = or(exp)
    def *(exp: Expr) = and(exp)
    def &(exp: Expr) = and(exp)
    def unary_- = unary_!
    def unary_~ = unary_!
    def unary_+ = this
}

case class Not(val exp: Expr) extends Expr {
  override def toString: String = s"!$exp"
}

case class Sym(val label: Char) extends Expr {
    override def toString() = s"$label"
}

class CompExpr extends Expr

case class Or(var subs: List[Expr] = List()) extends CompExpr {
  override def toString: String = subs.mkString("(", " | ", ")")
}

case class And(var subs: List[Expr] = List()) extends CompExpr {
  override def toString: String = subs.mkString("(", " & ", ")")
}

val x = Sym('X')
val y = Sym('Y')
println(x or (x and y)) // -> !X | !Y