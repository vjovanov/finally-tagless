// Our Trees
sealed trait Exp
case class Cst(v: Any) extends Exp
case class Lam(f: Exp => Exp) extends Exp
case class App(f: Exp, v: Exp) extends Exp
case class If(c: Exp, t: Exp, e: Exp) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Leq(lhs: Exp, rhs: Exp) extends Exp
case class Fix(f: (() => Exp) => Exp) extends Exp
// Dummy class to show holes in AST
case class Hole(x: Exp) extends Exp {
  override def toString = s"[$x]"
}

object C extends Symantics {
  type Repr[D,S] = Exp
  def int(c: Int): Repr[Int,Int] = Cst(c)
  def bool(c: Boolean): Repr[Boolean,Boolean] = Cst(c)
  def lam[A, B](f: SFun[A,B]): Repr[A => B, SFun[A,B]] = Lam(f)
  def app[A, B](f: RLam[A,B])(v: Repr[A,A]): Repr[B,B] = App(f,v)
  def _if[D,S](c: Repr[Boolean,Boolean])(t: =>Repr[D,S])(e: =>Repr[D,S]): Repr[D,S] = If(c, t, e)
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = Add(lhs, rhs)
  def mul(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = Mul(lhs, rhs)
  def leq(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Boolean,Boolean] = Leq(lhs, rhs)

  def fix[A,B](f: (() => RLam[A,B]) => RLam[A,B]): RLam[A,B] = Fix(f)

  override def hole[D,S](x: =>Repr[D,S]): Repr[D,S] = Hole(x)
  def eval[T](exp: Repr[T,T]): T = (exp match {
    case Cst(i) => i
    case App(a, b) => eval[Any=>Any](a)(eval(b))
    case Lam(f) => x: Any => eval[Any](f(Cst(x)))
    case If(c, t, e) => if (eval[Boolean](c)) eval(t) else eval(e)
    case Add(l, r) => eval[Int](l) + eval[Int](l)
    case Mul(l, r) => eval[Int](l) * eval[Int](l)
    case Leq(l, r) => eval[Int](l) <= eval[Int](r)
    case Fix(f) => ???
    case Hole(x) => eval(x)
  }).asInstanceOf[T]
}
