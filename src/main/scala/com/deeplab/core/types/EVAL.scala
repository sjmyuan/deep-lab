package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct
import com.deeplab.core.types.EXPR.EXPRTYPE

/**
  * Created by jiaming.shang on 5/7/17.
  */
trait EVAL[F[_]] {
  def eval(v: F[(Map[String, Int]) => Int]): (Map[String, Int]) => Int
}

object EVAL {
  implicit def valEval = new EVAL[VAL] {
    override def eval(v: VAL[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        v.v
      }
    }
  }

  implicit def varEval = new EVAL[VAR] {
    override def eval(v: VAR[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        x(v.name)
      }
    }
  }

  implicit def addEval = new EVAL[ADD] {
    override def eval(v: ADD[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        v.lv(x) + v.rv(x)
      }
    }
  }

  implicit def subEval = new EVAL[SUB] {
    override def eval(v: SUB[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        v.lv(x) - v.rv(x)
      }
    }
  }

  implicit def mulEval = new EVAL[MUL] {
    override def eval(v: MUL[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        v.lv(x) * v.rv(x)
      }
    }
  }

  implicit def divEval = new EVAL[DIV] {
    override def eval(v: DIV[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        v.lv(x) / v.rv(x)
      }
    }
  }

  implicit def expEval = new EVAL[EXP] {
    override def eval(v: EXP[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        Math.exp(v.v(x)) toInt
      }
    }
  }

  implicit def logEval = new EVAL[LOG] {
    override def eval(v: LOG[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        Math.log(v.v(x)) toInt
      }
    }
  }

  implicit def powEval = new EVAL[POW] {
    override def eval(v: POW[(Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      x: Map[String,Int] => {
        Math.pow(v.lv(x), v.rv(x)) toInt
      }
    }
  }

  implicit def coproductEval[F[_], G[_]](implicit feval: EVAL[F], geval: EVAL[G]) = new EVAL[Coproduct[F, G, ?]] {
    override def eval(v: Coproduct[F, G, (Map[String, Int]) => Int]): (Map[String, Int]) => Int = {
      v.run match {
        case l: Left[F[Map[String,Int] => Int], G[Map[String,Int] => Int]] => feval.eval(l.a)
        case r: Right[F[Map[String,Int] => Int], G[Map[String,Int] => Int]] => geval.eval(r.b)
      }
    }
  }

  def eval[F[_],A](expr: EXPR[F])(implicit feval: EVAL[F], ffunctor: Functor[F]): (Map[String,Int] => Int) = {
    EXPR.fold(expr)((v: F[(Map[String,Int]) => Int]) => feval.eval(v))
  }
}
