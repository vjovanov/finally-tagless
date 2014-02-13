
sealed trait Exp
case class Cst(v: Any) extends Exp
case class Lam(f: Exp => Exp) extends Exp
case class App(f: Exp, v: Exp) extends Exp
case class If(c: Exp, t: Exp, e: Exp) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
// Dummy class to show holes in AST
case class Hole(x: Exp) extends Exp {
  override def toString = s"[$x]"
}

trait Symantics { 
  type Repr[D,S]
  type SFun[A,B] = Repr[A,A] => Repr[B,B]
  def int(c: Int): Repr[Int,Int]
  def bool(c: Boolean): Repr[Boolean,Boolean]
  def lam[A, B](f: SFun[A,B]): Repr[A => B, SFun[A,B]]
  def app[A, B](f: Repr[A => B, SFun[A,B]])(v: Repr[A,A]): Repr[B,B]
  def _if[D,S](c: Repr[Boolean,Boolean])(t: Repr[D,S])(e: Repr[D,S]): Repr[D,S]
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int]
  def eval[T](exp: Repr[T,T]): T
}

object C extends Symantics {
  type Repr[D,S] = Exp
  def int(c: Int): Repr[Int,Int] = Cst(c)
  def bool(c: Boolean): Repr[Boolean,Boolean] = Cst(c)
  def lam[A, B](f: SFun[A,B]): Repr[A => B, SFun[A,B]] = Lam(f)
  def app[A, B](f: Repr[A => B, SFun[A,B]])(v: Repr[A,A]): Repr[B,B] = App(f,v)
  def _if[D,S](c: Repr[Boolean,Boolean])(t: Repr[D,S])(e: Repr[D,S]): Repr[D,S] = If(c, t, e)
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = Add(lhs, rhs)
  def hole[T](x: Repr[T,T]): Repr[T,T] = Hole(x)
  def eval[T](exp: Repr[T,T]): T = (exp match {
    case Cst(i) => i
    case App(a, b) => eval[Any=>Any](a)(eval(b))
    case Lam(f) => x: Any => eval[Any](f(Cst(x)))
    case If(c, t, e) => if (eval[Boolean](c)) eval(t) else eval(e)
    case Add(l, r) => eval[Int](l) + eval[Int](l)
    case Hole(x) => eval(x)
  }).asInstanceOf[T]
}

object R extends Symantics {
  type Repr[D,S] = D
  def int(c: Int): Repr[Int,Int] = c
  def bool(c: Boolean): Repr[Boolean,Boolean] = c
  def lam[A, B](f: SFun[A,B]): Repr[A => B, SFun[A,B]] = f
  def app[A, B](f: Repr[A => B, SFun[A,B]])(v: Repr[A,A]): Repr[B,B] = f(v)
  def _if[D,S](c: Repr[Boolean,Boolean])(t: Repr[D,S])(e: Repr[D,S]): Repr[D,S] = 
    if(c) t else e
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = lhs + rhs
  def eval[T](exp: Repr[T,T]): T = exp
}

object P extends Symantics {

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

  def lam[A, B](f: SFun[A,B]): Repr[A => B, SFun[A,B]] = {
    // need to ascribe here.
    val crepr: C.Repr[A => B, A => B] =
      C.lam((x: C.Repr[A,A]) => f(repr[A,A](x)).c)
    repr(crepr, f)
  }

  def app[A, B](f: Repr[A => B, SFun[A,B]])(v: Repr[A,A]): Repr[B,B] = f.r match {
    case Some(f) => f(v)
    case None    => repr(C.app(f.c)(v.c))
  }

  def _if[A,B](c: Repr[Boolean,Boolean])(t: Repr[A,B])(e: Repr[A,B]): Repr[A,B] = c.r match {
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
  def eval[T](exp: Repr[T,T]): T = exp.r getOrElse { C.eval(exp.c) }
  def hole[D,S](x: C.Repr[D,D]): Repr[D,S] = repr(C.hole(x))
}

object Test extends scala.App {
  def createProgram(c: Symantics)
    (hole1: c.Repr[Int,Int], hole2: c.Repr[Int,Int], hole3: c.Repr[Boolean, Boolean]): c.Repr[Int,Int] = {
    import c._
    app(lam((x: Repr[Int,Int]) => add(x, hole1)))(_if(hole3)(int(1))(add(hole2, int(4))))
  }

  val cp = createProgram(C)(C.int(1),C.int(1),C.bool(false))
  val rp = createProgram(R)(R.int(1),R.int(1),R.bool(false))

  val pp1 = createProgram(P)(P.int(1), P.hole(C.int(100)), P.bool(false))
  val pp2 = createProgram(P)(P.int(1), P.hole(C.int(100)), P.bool(true))
  val pp3 = createProgram(P)(P.int(1), P.int(100), P.hole(C.bool(true)))

  println(s"C: $cp = ${C.eval(cp)}")
  println(s"R: $rp")
  println(s"P: $pp1 = ${P.eval(pp1)}")
  println(s"P: $pp2 = ${P.eval(pp2)}")
  println(s"P: $pp3 = ${P.eval(pp3)}")
}
