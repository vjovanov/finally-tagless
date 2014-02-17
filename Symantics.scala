import language.higherKinds

trait Symantics { 
  type Repr[D,S]

  // Helper Types
  type SFun[A,B] = Repr[A,A] => Repr[B,B]
  type RLam[A,B] = Repr[A => B, SFun[A,B]]

  def int(c: Int): Repr[Int,Int]
  def bool(c: Boolean): Repr[Boolean,Boolean]
  def lam[A, B](f: SFun[A,B]): RLam[A,B]
  def app[A, B](f: RLam[A,B])(v: Repr[A,A]): Repr[B,B]
  def _if[D,S](c: Repr[Boolean,Boolean])(t: =>Repr[D,S])(e: =>Repr[D,S]): Repr[D,S]
  def add(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int]
  def mul(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Int,Int]
  def leq(lhs: Repr[Int,Int], rhs: Repr[Int,Int]): Repr[Boolean,Boolean]

  // For partial evaluation, we can only allow fix on lambdas 
  def fix[A,B](f: (() => RLam[A,B]) => RLam[A,B]): RLam[A,B]
 
  // Helper to show partial evaluation actually works
  def hole[D,S](exp: =>Repr[D,S]): Repr[D,S] = exp

  def eval[T](exp: Repr[T,T]): T

  object DSL {
    implicit class IntOps(val lhs: Repr[Int,Int]) {
      def +(rhs: Repr[Int,Int]) = add(lhs, rhs)
      def *(rhs: Repr[Int,Int]) = mul(lhs, rhs)
      def <=(rhs: Repr[Int,Int]) = leq(lhs, rhs)
    }
    
    implicit class FunOps[A,B](val f: RLam[A,B]) {
      def apply(v: Repr[A,A]) = app(f)(v)
    }
  }
}
