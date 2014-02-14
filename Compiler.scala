// Our Trees
sealed trait Exp

// Placeholder to pose variables
class PH extends Exp

// De-Brujin encoded var
case class Var(n: Int) extends Exp
case class Cst(v: Any) extends Exp
case class Lam(f: Exp) extends Exp
// Note that a fix is also a lambda for De-Brujin
case class Fix(f: Exp) extends Exp
case class App(f: Exp, v: Exp) extends Exp
case class If(c: Exp, t: Exp, e: Exp) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Leq(lhs: Exp, rhs: Exp) extends Exp
// Dummy class to show holes in AST
case class Hole(x: Exp) extends Exp {
  override def toString = s"[$x]"
}

object C extends Symantics {
  type Repr[D,S] = Exp
  def int(c: Int): Repr[Int,Int] = Cst(c)
  def bool(c: Boolean): Repr[Boolean,Boolean] = Cst(c)
  def lam[A, B](f: SFun[A,B]): Repr[A => B, SFun[A,B]] = {
    val ph = new PH
    Lam(resolvePH(ph, f(ph)))
  }
  def app[A, B](f: RLam[A,B])(v: Repr[A,A]): Repr[B,B] = App(f,v)
  def _if[D,S](c: Repr[Boolean,Boolean])(t: =>Repr[D,S])(e: =>Repr[D,S]): Repr[D,S] = If(c, t, e)
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = Add(lhs, rhs)
  def mul(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = Mul(lhs, rhs)
  def leq(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Boolean,Boolean] = Leq(lhs, rhs)

  def fix[A,B](f: (() => RLam[A,B]) => RLam[A,B]): RLam[A,B] = {
    val ph = new PH
    Fix(resolvePH(ph, f(() => ph)))
  }

  /** increase count in De-Brujin vars */
  private def resolvePH(ph: PH, e: Exp): Exp =
    new VarResolveTransformer(ph).transform(e)

  /** decrease count in De-Brujin vars */
  private def popVar(body: Exp, v: Exp) =
    new VarPopTransformer(v).transform(body)


  override def hole[D,S](x: =>Repr[D,S]): Repr[D,S] = Hole(x)

  def eval[T](exp: Repr[T,T]): T = (exp match {
    case Cst(i) => i
    case App(fun, arg) =>
      // We do lazy eval
      eval[T](popVar(fun, arg))
    case Lam(body) => eval[T](body)
    case If(c, t, e) => if (eval[Boolean](c)) eval(t) else eval(e)
    case Add(l, r) => eval[Int](l) + eval[Int](l)
    case Mul(l, r) => eval[Int](l) * eval[Int](l)
    case Leq(l, r) => eval[Int](l) <= eval[Int](r)
    case Fix(f) => ???
    case Hole(x) => eval(x)
    case Var(_)  => sys.error("Unbound variable. You die!")
    case _: PH   => sys.error("Unbound placeholder. You die!")
  }).asInstanceOf[T]

  class VarPopTransformer(val v: Exp) extends Transformer {
    override def transform(e: Exp) = e match {
      case Var(0) => super.transform(v)
      case Var(i) => Var(i-1)
      case _ => super.transform(e)
    }
  }

  class VarResolveTransformer(ph: PH) extends Transformer {
    var depth = 0
    override def transform(e: Exp) = e match {
      case `ph`   => Var(depth)
      case Lam(_) | Fix(_) =>
        descend { super.transform(e) }
      case _ =>
        super.transform(e)
    }

    private def descend[T](body: =>T) = {
      depth += 1
      val res = body
      depth -= 1
      res
    }
  }

  class Transformer {
    def transform(e: Exp): Exp = e match {
      case Var(_) | Cst(_) | _ :PH => e
      case Lam(f)          => Lam(transform(f))
      case App(fun, body)  => App(transform(fun), transform(body))
      case Fix(f)          => Fix(transform(f))
      case If(c,t,e)       => If(transform(c), transform(t), transform(e))
      case Add(lhs, rhs)   => Add(transform(lhs), transform(rhs))
      case Mul(lhs, rhs)   => Mul(transform(lhs), transform(rhs))
      case Leq(lhs, rhs)   => Leq(transform(lhs), transform(rhs))
      case Hole(x)         => Hole(transform(x))
    }
  }
}
