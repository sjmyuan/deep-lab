package com.deeplab.core.types

import com.deeplab.core.types.EXPR.EXPRTYPE
import EXPR._
import cats.Functor
import cats.data.Coproduct
import cats.free.Inject

/**
  * Created by jiaming.shang on 5/26/17.
  */
trait Optimize[F[_],G[_]] {
  def optimize(v: F[EXPR[G]]): EXPR[G]
}

object Optimize {
  implicit def varOptimize[G[_]](implicit I:Inject[VAR,G]) = new Optimize[VAR,G] {
    override def optimize(v: VAR[EXPR[G]]): EXPR[G] = {
      v match {
        case x: INTVAL[_] if x.v.v == 0 => EXPR.inject[VAR, G](ZERO("", List()))
        case x: INTVAL[_] if x.v.v == 1 => EXPR.inject[VAR, G](ONE("", List()))
        case x: FLOATVAL[_] if x.v.v == 0 => EXPR.inject[VAR, G](ZERO("", List()))
        case x: FLOATVAL[_] if x.v.v == 1 => EXPR.inject[VAR, G](ONE("", List()))
        case x: DOUBLEVAL[_] if x.v.v == 0 => EXPR.inject[VAR, G](ZERO("", List()))
        case x: DOUBLEVAL[_] if x.v.v == 1 => EXPR.inject[VAR, G](ONE("", List()))
        case _ => EXPR.inject[VAR, G](v)
      }

    }
  }

  implicit def addOptimize[G[_]](implicit I1:Inject[VAR,G],I2:Inject[ADD,G],I3:Inject[MUL,G]) = new Optimize[ADD,G] {
    override def optimize(v: ADD[EXPR[G]]): EXPR[G] = {
      if (isZeroExpr(v.lv)) {
        v.rv
      } else if (isZeroExpr(v.rv)) {
        v.lv
      } else if (v.lv == v.rv) {
        mulExpr[G](EXPR.inject[VAR, G](DOUBLEVAL(2, List(), "")),v.lv)
      } else {
        EXPR.inject[ADD, G](v)
      }
    }
  }

  implicit def subOptimize[G[_]](implicit I1:Inject[VAR,G],I2:Inject[SUB,G],I3:Inject[NEG,G]) = new Optimize[SUB,G] {
    override def optimize(v: SUB[EXPR[G]]): EXPR[G] = {
      if (v.lv == v.rv)
        EXPR.inject[VAR, G](ZERO("", List()))
      else if (isZeroExpr(v.lv))
        negExpr(v.rv)
      else if (isZeroExpr(v.rv))
        v.lv
      else
        EXPR.inject[SUB, G](v)
    }
  }

  implicit def mulOptimize[G[_]](implicit I1:Inject[VAR,G],I2:Inject[MUL,G])= new Optimize[MUL,G] {
    override def optimize(v: MUL[EXPR[G]]): EXPR[G] = {
      if (isZeroExpr(v.lv) || isZeroExpr(v.rv))
        EXPR.inject[VAR, G](ZERO("", List()))
      else if (isOneExpr(v.lv))
        v.rv
      else if (isOneExpr(v.rv))
        v.lv
      else
        EXPR.inject[MUL, G](v)
    }
  }

  implicit def divOptimize[G[_]](implicit I1:Inject[VAR,G],I2:Inject[DIV,G]) = new Optimize[DIV,G] {
    override def optimize(v: DIV[EXPR[G]]): EXPR[G] = {
      if (v.lv == v.rv)
        EXPR.inject[VAR, G](ONE("", List()))
      else if (isZeroExpr(v.lv) || isOneExpr(v.rv))
        v.lv
      else
        EXPR.inject[DIV, G](v)
    }
  }

  implicit def powOptimize[G[_]](implicit I1:Inject[VAR,G],I2:Inject[POW,G]) = new Optimize[POW,G] {
    override def optimize(v: POW[EXPR[G]]): EXPR[G] = {
      if (isZeroExpr(v.lv))
        EXPR.inject[VAR, G](ZERO("", List()))
      else if (isZeroExpr(v.rv) || isOneExpr(v.lv))
        EXPR.inject[VAR, G](ONE("", List()))
      else
        EXPR.inject[POW, G](v)
    }
  }

  implicit def logOptimize[G[_]](implicit I1:Inject[VAR,G],I2:Inject[LOG,G]) = new Optimize[LOG,G] {
    override def optimize(v: LOG[EXPR[G]]): EXPR[G] = {
      EXPR.inject[LOG, G](v)
    }
  }

  implicit def expOptimzie[G[_]](implicit I1:Inject[VAR,G],I2:Inject[EXP,G]) = new Optimize[EXP,G] {
    override def optimize(v: EXP[EXPR[G]]): EXPR[G] = {
      if (isZeroExpr(v.v))
        EXPR.inject[VAR, G](ONE("", List()))
      else
        EXPR.inject[EXP, G](v)
    }
  }

  implicit def negOptimize[G[_]](implicit I1:Inject[NEG,G],I2:Inject[VAR,G]) = new Optimize[NEG,G] {
    override def optimize(v: NEG[EXPR[G]]): EXPR[G] = {
      if (isZeroExpr(v.v))
        v.v
      else
        EXPR.inject[NEG, G](v)
    }
  }

  implicit def coproductOptimize[F[_], G[_],H[_], A](implicit fopt: Optimize[F,H], gopt: Optimize[G,H]) = new Optimize[Coproduct[F, G, ?],H] {
    override def optimize(v: Coproduct[F, G, EXPR[H]]): EXPR[H] = {
      v.run match {
        case l: Left[F[EXPR[H]], G[EXPR[H]]] => fopt.optimize(l.a)
        case r: Right[F[EXPR[H]], G[EXPR[H]]] => gopt.optimize(r.b)
      }
    }
  }

  def optimize[F[_]](expr: EXPR[F])(implicit f: Functor[F], fopt: Optimize[F,F]): EXPR[F] = {
    EXPR.fold(expr) { v: F[EXPR[F]] => fopt.optimize(v) }
  }
}
