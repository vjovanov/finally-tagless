object C extends Symantics {

  // Our Trees
  sealed trait Exp
  
  sealed trait Value extends Exp
  
  // Placeholder to pose variables
  class PH extends Exp
  
  // De-Brujin encoded var
  case class Var(n: Int) extends Exp
  case class Cst(v: Any) extends Value
  case class Lam(f: Exp) extends Value
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

  override def hole[D,S](x: =>Repr[D,S]): Repr[D,S] = Hole(x)


  def eval[T](exp: Repr[T,T]): T = (reduce(exp) match {
    case Cst(i) => i
    case _ =>
      sys.error("Wont happen. Repr of a lambda does not have right signature")
  }).asInstanceOf[T]

  /** big step reduction */
  private def reduce(exp: Exp): Value = exp match {
    case v: Value => v
    case App(fun, arg) => reduce(fun) match {
      case Lam(body) =>
        // We do lazy eval
        reduce(subst(body, arg))
      case _ => stuck
    }
    case If(c, t, e) => reduce(c) match {
      case Cst(true)  => reduce(t)
      case Cst(false) => reduce(e)
      case _ => stuck
    }
    case Add(l, r) => (reduce(l), reduce(r)) match {
      case (Cst(x: Int), Cst(y: Int)) => Cst(x + y)
      case _ => stuck
    }
    case Mul(l, r) => (reduce(l), reduce(r)) match {
      case (Cst(x: Int), Cst(y: Int)) => Cst(x * y)
      case _ => stuck
    }
    case Leq(l, r) => (reduce(l), reduce(r)) match {
      case (Cst(x: Int), Cst(y: Int)) => Cst(x <= y)
      case _ => stuck
    }
    case Fix(body) =>
      // Lazily replace ourselves
      reduce(subst(body, exp))
    case Hole(x) => reduce(x)
    case Var(_)  => sys.error("Unbound variable. You die!")
    case _: PH   => sys.error("Unbound placeholder. You die!")
  }

  private def stuck = sys.error("stuck. you die!")

  private def resolvePH(ph: PH, e: Exp): Exp =
    new VarResolveTransformer(ph).transform(e)

  private def subst(body: Exp, v: Exp) =
    new VarSubstTransformer(v).transform(body)

  private class VarSubstTransformer(val v: Exp) extends Transformer {
    override def transform(e: Exp) = e match {
      case Var(i) if i == depth =>
        super.transform(v)
      case _ =>
        super.transform(e)
    }
  }

  private class VarResolveTransformer(ph: PH) extends Transformer {
    override def transform(e: Exp) = e match {
      case `ph`   => Var(depth)
      case _ =>
        super.transform(e)
    }
  }

  private class Transformer {
    var depth = 0

    protected def descend[T](body: =>T) = {
      depth += 1
      val res = body
      depth -= 1
      res
    }

    def transform(e: Exp): Exp = e match {
      case Var(_) | Cst(_) | _ :PH => e
      case Lam(f)          => descend { Lam(transform(f)) }
      case App(fun, body)  => App(transform(fun), transform(body))
      case Fix(f)          => descend { Fix(transform(f)) }
      case If(c,t,e)       => If(transform(c), transform(t), transform(e))
      case Add(lhs, rhs)   => Add(transform(lhs), transform(rhs))
      case Mul(lhs, rhs)   => Mul(transform(lhs), transform(rhs))
      case Leq(lhs, rhs)   => Leq(transform(lhs), transform(rhs))
      case Hole(x)         => Hole(transform(x))
    }
  }
}
