object R extends Symantics {
  type Repr[D,S] = D
  def int(c: Int): Repr[Int,Int] = c
  def bool(c: Boolean): Repr[Boolean,Boolean] = c
  def lam[A, B](f: SFun[A,B]): RLam[A,B] = f
  def app[A, B](f: Repr[A => B, SFun[A,B]])(v: Repr[A,A]): Repr[B,B] = f(v)
  def _if[D,S](c: Repr[Boolean,Boolean])(t: =>Repr[D,S])(e: =>Repr[D,S]): Repr[D,S] = 
    if(c) t else e
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = lhs + rhs
  def mul(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int] = lhs * rhs
  def leq(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Boolean,Boolean] = lhs <= rhs
  def fix[A,B](f: (() => RLam[A,B]) => RLam[A,B]): RLam[A,B] = {
    def self: RLam[A,B] = { f(() => self) }
    self
  }
  def eval[T](exp: Repr[T,T]): T = exp
}
