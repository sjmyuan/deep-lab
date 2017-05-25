package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct

/**
  * Created by jiaming.shang on 5/17/17.
  */


trait TypeInspect[F[_]] {
  def inspect(v: F[Algebra]): Algebra
}

object TypeInspect {
  implicit def varInspect = new TypeInspect[VAR] {
    override def inspect(v: VAR[Algebra]): Algebra = {
      v match {
        case _: DOUBLEVAR[Algebra] => new DoubleAlgebra
        case _: DOUBLEVAL[Algebra] => new DoubleAlgebra
        case _: INTVAR[Algebra] => new IntAlgebra
        case _: INTVAL[Algebra] => new IntAlgebra
        case _ => new DoubleAlgebra
      }
    }
  }

  implicit def negInspect = new TypeInspect[NEG] {
    override def inspect(v: NEG[Algebra]): Algebra = {
      v.v
    }
  }

  implicit def addInspect = new TypeInspect[ADD] {
    override def inspect(v: ADD[Algebra]): Algebra = {
      if (v.lv.Index > v.rv.Index)
        v.lv
      else
        v.rv
    }
  }

  implicit def subInspect = new TypeInspect[SUB] {
    override def inspect(v: SUB[Algebra]): Algebra = {
      if (v.lv.Index > v.rv.Index)
        v.lv
      else
        v.rv
    }
  }

  implicit def mulInspect = new TypeInspect[MUL] {
    override def inspect(v: MUL[Algebra]): Algebra = {
      if (v.lv.Index > v.rv.Index)
        v.lv
      else
        v.rv
    }
  }

  implicit def divInspect = new TypeInspect[DIV] {
    override def inspect(v: DIV[Algebra]): Algebra = {
      if (v.lv.Index > v.rv.Index)
        v.lv
      else
        v.rv
    }
  }

  implicit def expInspect = new TypeInspect[EXP] {
    override def inspect(v: EXP[Algebra]): Algebra = {
      v.v.toDouble()
    }
  }

  implicit def logInspect = new TypeInspect[LOG] {
    override def inspect(v: LOG[Algebra]): Algebra = {
      v.v.toDouble()
    }
  }

  implicit def powInspect = new TypeInspect[POW] {
    override def inspect(v: POW[Algebra]): Algebra = {
      if(v.lv.Index()>v.rv.Index())
        v.lv.toDouble()
      else
        v.rv.toDouble()
    }
  }

  implicit def coproductInspect[F[_], G[_]](implicit f: TypeInspect[F], g: TypeInspect[G]) = new TypeInspect[Coproduct[F, G, ?]] {
    override def inspect(v: Coproduct[F, G, Algebra]): Algebra = {
      v.run match {
        case l: Left[F[Algebra], G[Algebra]] => f.inspect(l.a)
        case r: Right[F[Algebra], G[Algebra]] => g.inspect(r.b)
      }
    }
  }

  def inspect[F[_]](expr: EXPR[F])(implicit f: TypeInspect[F], ffunctor: Functor[F]): Algebra = {
    EXPR.fold(expr)((v: F[Algebra]) => f.inspect(v))
  }
}

