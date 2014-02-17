object Test extends scala.App {

  def createProgram(c: Symantics)
    (hole1: c.Repr[Int,Int], hole2: c.Repr[Int,Int], hole3: c.Repr[Boolean, Boolean]): c.Repr[Int,Int] = {
    import c._
    app(lam((x: Repr[Int,Int]) => add(x, hole1)))(_if(hole3)(int(1))(add(hole2, int(4))))
  }

  val cp = createProgram(C)(C.int(1),C.int(1),C.bool(false))
  val rp = createProgram(R)(R.int(1),R.int(1),R.bool(false))

  val pp1 = createProgram(P)(P.int(1), P.hole(P.int(100)), P.bool(false))
  val pp2 = createProgram(P)(P.int(1), P.hole(P.int(100)), P.bool(true))
  val pp3 = createProgram(P)(P.int(1), P.int(100), P.hole(P.bool(true)))

  println(s"C: $cp = ${C.eval(cp)}")
  println(s"R: $rp")
  println(s"P: $pp1 = ${P.eval(pp1)}")
  println(s"P: $pp2 = ${P.eval(pp2)}")
  println(s"P: $pp3 = ${P.eval(pp3)}")


  /** a factorial function that maps before multiplication */
  def mapFact(c: Symantics)(cMap: c.RLam[Int,Int])(vMap: c.RLam[Int,Int]) = {
    import c._
    import DSL._
    fix[Int,Int]( self =>
      lam[Int,Int](n =>
        _if(cMap(n) <= int(0)) { int(1) } {
          vMap(n) * self()(n + int(-1))
        }
      )
    )
  }

  def ident(c: Symantics) = c.lam[Int,Int](n => n)

  def holeAbove(c: Symantics)(v: Int) = {
    import c._
    import DSL._
    lam[Int,Int](n =>
      _if(int(v) <= n)(hole(n))(n)
    )
  }

  def holeBelow(c: Symantics)(v: Int) = {
    import c._
    lam[Int,Int](n =>
      _if(leq(int(v),n))(n)(hole(n))
    )
  }

  def testFix(c: Symantics) = {
    import c._
    val id = ident(c)
    val abid = holeAbove(c)(6)
    val beid = holeBelow(c)(5)
    app(mapFact(c)(abid)(abid))(int(7))
  }

  val fProg = testFix(P)

  println(fProg)
  println(P.eval(fProg))
  println(R.eval(testFix(R)))
}
