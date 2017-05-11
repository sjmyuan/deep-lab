package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct
import com.deeplab.core.types.EXPR.EXPRTYPE

/**
  * Created by jiaming.shang on 5/7/17.
  */
trait EVAL[F[_]] {
  def eval(v: F[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double]
}

object EVAL {
  implicit def valEval = new EVAL[VAL] {
    override def eval(v: VAL[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.v
      }
    }
  }

  implicit def varEval = new EVAL[VAR] {
    override def eval(v: VAR[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        x(v.name)
      }
    }
  }

  implicit def addEval = new EVAL[ADD] {
    override def eval(v: ADD[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.lv(x) + v.rv(x)
      }
    }
  }

  implicit def subEval = new EVAL[SUB] {
    override def eval(v: SUB[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.lv(x) - v.rv(x)
      }
    }
  }

  implicit def mulEval = new EVAL[MUL] {
    override def eval(v: MUL[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.lv(x) * v.rv(x)
      }
    }
  }

  implicit def divEval = new EVAL[DIV] {
    override def eval(v: DIV[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.lv(x) / v.rv(x)
      }
    }
  }

  implicit def expEval = new EVAL[EXP] {
    override def eval(v: EXP[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.v(x).exp
      }
    }
  }

  implicit def logEval = new EVAL[LOG] {
    override def eval(v: LOG[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.v(x).log
      }
    }
  }

  implicit def powEval = new EVAL[POW] {
    override def eval(v: POW[(Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      x: Map[String,Variable[Double]] => {
        v.lv(x) pow v.rv(x)
      }
    }
  }

  implicit def coproductEval[F[_], G[_]](implicit feval: EVAL[F], geval: EVAL[G]) = new EVAL[Coproduct[F, G, ?]] {
    override def eval(v: Coproduct[F, G, (Map[String, Variable[Double]]) => Variable[Double]]): (Map[String, Variable[Double]]) => Variable[Double] = {
      v.run match {
        case l: Left[F[Map[String,Variable[Double]] => Variable[Double]], G[Map[String,Variable[Double]] => Variable[Double]]] => feval.eval(l.a)
        case r: Right[F[Map[String,Variable[Double]] => Variable[Double]], G[Map[String,Variable[Double]] => Variable[Double]]] => geval.eval(r.b)
      }
    }
  }

  def eval[F[_]](expr: EXPR[F])(implicit feval: EVAL[F], ffunctor: Functor[F]): (Map[String,Variable[Double]] => Variable[Double]) = {
    EXPR.fold(expr)((v: F[(Map[String,Variable[Double]]) => Variable[Double]]) => feval.eval(v))
  }
}
