package com.deeplab.core.types

import com.deeplab.core.types.EXPR.EXPRTYPE
import EXPR._
import cats.Functor
import cats.data.Coproduct
import cats.free.Inject

/**
  * Created by jiaming.shang on 5/26/17.
  */
trait Optimize[F[_]] {
  def optimize(v: F[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE]
}

object Optimize {
  implicit def varOptimize = new Optimize[VAR] {
    override def optimize(v: VAR[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      v match {
        case x: INTVAL[_] if x.v.v == 0 => inject[VAR, EXPRTYPE](ZERO("", List()))
        case x: INTVAL[_] if x.v.v == 1 => inject[VAR, EXPRTYPE](ONE("", List()))
        case x: FLOATVAL[_] if x.v.v == 0 => inject[VAR, EXPRTYPE](ZERO("", List()))
        case x: FLOATVAL[_] if x.v.v == 1 => inject[VAR, EXPRTYPE](ONE("", List()))
        case x: DOUBLEVAL[_] if x.v.v == 0 => inject[VAR, EXPRTYPE](ZERO("", List()))
        case x: DOUBLEVAL[_] if x.v.v == 1 => inject[VAR, EXPRTYPE](ONE("", List()))
        case _ => inject[VAR, EXPRTYPE](v)
      }

    }
  }

  implicit def addOptimize = new Optimize[ADD] {
    override def optimize(v: ADD[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      if (isZeroExpr(v.lv)) {
        v.rv
      } else if (isZeroExpr(v.rv)) {
        v.lv
      } else if (v.lv == v.rv) {
        2 * v.lv
      } else {
        inject[ADD, EXPRTYPE](v)
      }
    }
  }

  implicit def subOptimize = new Optimize[SUB] {
    override def optimize(v: SUB[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      if (v.lv == v.rv)
        inject[VAR, EXPRTYPE](ZERO("", List()))
      else if (isZeroExpr(v.lv))
        negExpr(v.rv)
      else if (isZeroExpr(v.rv))
        v.lv
      else
        inject[SUB, EXPRTYPE](v)
    }
  }

  implicit def mulOptimize = new Optimize[MUL] {
    override def optimize(v: MUL[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      if (isZeroExpr(v.lv) || isZeroExpr(v.rv))
        v.lv
      else if (isOneExpr(v.lv))
        v.rv
      else if (isOneExpr(v.rv))
        v.lv
      else
        inject[MUL, EXPRTYPE](v)
    }
  }

  implicit def divOptimize = new Optimize[DIV] {
    override def optimize(v: DIV[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      if (v.lv == v.rv)
        inject[VAR, EXPRTYPE](ONE("", List()))
      else if (isZeroExpr(v.lv) || isOneExpr(v.rv))
        v.lv
      else
        inject[DIV, EXPRTYPE](v)
    }
  }

  implicit def powOptimize = new Optimize[POW] {
    override def optimize(v: POW[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      if (isZeroExpr(v.lv))
        inject[VAR, EXPRTYPE](ZERO("", List()))
      else if (isZeroExpr(v.rv) || isOneExpr(v.lv))
        inject[VAR, EXPRTYPE](ONE("", List()))
      else
        inject[POW, EXPRTYPE](v)
    }
  }

  implicit def logOptimize = new Optimize[LOG] {
    override def optimize(v: LOG[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      inject[LOG, EXPRTYPE](v)
    }
  }

  implicit def expOptimzie = new Optimize[EXP] {
    override def optimize(v: EXP[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      if (isZeroExpr(v.v))
        inject[VAR, EXPRTYPE](ONE("", List()))
      else
        inject[EXP, EXPRTYPE](v)
    }
  }

  implicit def negOptimize = new Optimize[NEG] {
    override def optimize(v: NEG[EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      inject[NEG, EXPRTYPE](v)
    }
  }

  implicit def coproductOptimize[F[_], G[_], A](implicit fopt: Optimize[F], gopt: Optimize[G]) = new Optimize[Coproduct[F, G, ?]] {
    override def optimize(v: Coproduct[F, G, EXPR[EXPRTYPE]]): EXPR[EXPRTYPE] = {
      v.run match {
        case l: Left[F[EXPR[EXPRTYPE]], G[EXPR[EXPRTYPE]]] => fopt.optimize(l.a)
        case r: Right[F[EXPR[EXPRTYPE]], G[EXPR[EXPRTYPE]]] => gopt.optimize(r.b)
      }
    }
  }

  def optimize[F[_]](expr: EXPR[F])(implicit f: Functor[F], fopt: Optimize[F]): EXPR[EXPRTYPE] = {
    EXPR.fold(expr) { v: F[EXPR[EXPRTYPE]] => fopt.optimize(v) }
  }
}
