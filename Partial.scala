object P extends Symantics {

  val maxFixEvalDepth = 10

  case class Repr[D,S](val c: C.Repr[D,D], val r: Option[R.Repr[S,S]]) {
    override def toString = r match {
      case Some(r) => s"Folded : $r"
      case None    => s"Partial: $c"
    }
  }

  private def repr[D,S](c: C.Repr[D,D]) = Repr[D,S](c, None)
  private def repr[D,S](c: C.Repr[D,D], r: R.Repr[S,S]) = Repr(c, Some(r))

  def int(c: Int): Repr[Int,Int] = repr(C.int(c), R.int(c))
  def bool(c: Boolean): Repr[Boolean,Boolean] = repr(C.bool(c), R.bool(c))

  def lam[A, B](f: SFun[A,B]): RLam[A,B] = {
    // need to ascribe here.
    val crepr: C.Repr[A => B, A => B] =
      C.lam((x: C.Repr[A,A]) => f(repr[A,A](x)).c)
    repr(crepr, f)
  }

  def app[A, B](f: Repr[A => B, SFun[A,B]])(v: Repr[A,A]): Repr[B,B] = f.r match {
    case Some(f) => f(v)
    case None    => repr(C.app(f.c)(v.c))
  }

  def _if[A,B](c: Repr[Boolean,Boolean])(t: =>Repr[A,B])(e: =>Repr[A,B]): Repr[A,B] = c.r match {
    case Some(true)  => t
    case Some(false) => e
    case None        => repr(C._if(c.c)(t.c)(e.c))
  }

  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = (lhs.r, rhs.r) match {
    case (Some(v1), Some(v2)) =>
      repr(C.int(v1 + v2), R.add(v1, v2))
    case (Some(0), _) =>
      repr(rhs.c)
    case (_, Some(0)) =>
      repr(lhs.c)
    case _ => repr(C.add(lhs.c, rhs.c))
  }

  def mul(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = (lhs.r, rhs.r) match {
    case (Some(v1), Some(v2)) =>
      repr(C.int(v1 * v2), R.mul(v1, v2))
    case (Some(0), _) | (_, Some(0)) =>
      repr(C.int(0), R.int(0))
    case (Some(1), _) =>
      repr(rhs.c)
    case (_, Some(1)) =>
      repr(lhs.c)
    case _ => repr(C.mul(lhs.c, rhs.c))
  }

  def leq(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Boolean,Boolean] = (lhs.r, rhs.r) match {
    case (Some(v1), Some(v2)) =>
      repr(C.bool(v1 <= v2), R.leq(v1, v2))
    case _ =>
      repr(C.leq(lhs.c, rhs.c))
  }

  def fix[A,B](f: (() => RLam[A,B]) => RLam[A,B]): RLam[A,B] = {
    def self(d: Int): RLam[A,B] = {
      if (d <= maxFixEvalDepth)
        f(() => self(d+1))
      else {
        val cfix = C.fix[A,B] { cSelf =>
          f(() => repr(cSelf())).c 
        }
        repr(cfix)
      }
    }
    self(0)
  }

  def eval[T](exp: Repr[T,T]): T = exp.r getOrElse { C.eval(exp.c) }
  override def hole[D,S](exp: =>Repr[D,S]): Repr[D,S] = {
    repr(C.hole(exp.c))
  }
}
