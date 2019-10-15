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
        case Or(subs) => return Or(Set(this, exp))
        case _ => return Or(Set(this, exp))
    }

    def and(exp: Expr): Expr =
    exp match {
        case And(subs) => return And(subs + this)
        case _ => return And(Set(this, exp))
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
// Composites or & and use sets, removing immediate repetitions

case class Or(var subs: Set[Expr] = Set()) extends CompExpr {
  override def toString: String = subs.mkString("(", " | ", ")")
  
  override def or(exp: Expr): Expr =
  exp match {
    case Or(subp) => return Or(subp ++ subs)
    case _ => return Or(subs + exp)
  }
}

case class And(var subs: Set[Expr] = Set()) extends CompExpr {
  override def toString: String = subs.mkString("(", " & ", ")")
  
  override def and(exp: Expr): Expr =
  exp match {
    case And(subp) => return And(subp ++ subs)
    case _ => return And(subs + exp)
  }
}

val x = Sym('X')
val y = Sym('Y')
println(x or x or (x and y)) // -> (X | (X & Y))