package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct
import com.deeplab.core.types.EXPR.EXPRTYPE
import Variable._
import TypeInspect._
import com.deeplab.core.types.EVAL.AlgebraFunction

/**
  * Created by jiaming.shang on 5/7/17.
  */
trait EVAL[F[_], A] {
  def eval(v: F[AlgebraFunction[A]]): AlgebraFunction[A]
}

object EVAL {
  type AlgebraFunction[A] = Map[String, Variable[A]] => Variable[A]

  implicit def varIntEval = new EVAL[VAR, Int] {
    override def eval(v: VAR[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        v match {
          case c: INTVAL[AlgebraFunction[Int]] => c.v
          case y: INTVAR[AlgebraFunction[Int]] => x(y.name)
        }
      }
    }
  }

  implicit def addIntEval = new EVAL[ADD, Int] {
    override def eval(v: ADD[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        v.lv(x) + v.rv(x)
      }
    }
  }

  implicit def subIntEval = new EVAL[SUB, Int] {
    override def eval(v: SUB[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        v.lv(x) - v.rv(x)
      }
    }
  }

  implicit def mulIntEval = new EVAL[MUL, Int] {
    override def eval(v: MUL[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        v.lv(x) * v.rv(x)
      }
    }
  }

  implicit def divIntEval = new EVAL[DIV, Int] {
    override def eval(v: DIV[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        v.lv(x) / v.rv(x)
      }
    }
  }

  implicit def expIntEval = new EVAL[EXP, Int] {
    override def eval(v: EXP[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        null
      }
    }
  }

  implicit def logIntEval = new EVAL[LOG, Int] {
    override def eval(v: LOG[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        null
      }
    }
  }

  implicit def powIntEval = new EVAL[POW, Int] {
    override def eval(v: POW[AlgebraFunction[Int]]): AlgebraFunction[Int] = {
      x: Map[String, Variable[Int]] => {
        null
      }
    }
  }

  implicit def varFloatEval = new EVAL[VAR, Float] {
    override def eval(v: VAR[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        v match {
          case c: FLOATVAL[AlgebraFunction[Float]] => c.v
          case y: FLOATVAR[AlgebraFunction[Float]] => x(y.name)
        }
      }
    }
  }

  implicit def addFloatEval = new EVAL[ADD, Float] {
    override def eval(v: ADD[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        v.lv(x) + v.rv(x)
      }
    }
  }

  implicit def subFloatEval = new EVAL[SUB, Float] {
    override def eval(v: SUB[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        v.lv(x) - v.rv(x)
      }
    }
  }

  implicit def mulFloatEval = new EVAL[MUL, Float] {
    override def eval(v: MUL[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        v.lv(x) * v.rv(x)
      }
    }
  }

  implicit def divFloatEval = new EVAL[DIV, Float] {
    override def eval(v: DIV[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        v.lv(x) / v.rv(x)
      }
    }
  }

  implicit def expFloatEval = new EVAL[EXP, Float] {
    override def eval(v: EXP[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        expFloat(v.v(x))
      }
    }
  }

  implicit def logFloatEval = new EVAL[LOG, Float] {
    override def eval(v: LOG[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        logFloat(v.v(x))
      }
    }
  }

  implicit def powFloatEval = new EVAL[POW, Float] {
    override def eval(v: POW[AlgebraFunction[Float]]): AlgebraFunction[Float] = {
      x: Map[String, Variable[Float]] => {
        powFloat(v.lv(x), v.rv(x))
      }
    }
  }
  implicit def varEval = new EVAL[VAR, Double] {
    override def eval(v: VAR[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        v match {
          case c: DOUBLEVAL[AlgebraFunction[Double]] => c.v
          case y: DOUBLEVAR[AlgebraFunction[Double]] => x(y.name)
        }
      }
    }
  }

  implicit def addEval = new EVAL[ADD, Double] {
    override def eval(v: ADD[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        v.lv(x) + v.rv(x)
      }
    }
  }

  implicit def subEval = new EVAL[SUB, Double] {
    override def eval(v: SUB[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        v.lv(x) - v.rv(x)
      }
    }
  }

  implicit def mulEval = new EVAL[MUL, Double] {
    override def eval(v: MUL[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        v.lv(x) * v.rv(x)
      }
    }
  }

  implicit def divEval = new EVAL[DIV, Double] {
    override def eval(v: DIV[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        v.lv(x) / v.rv(x)
      }
    }
  }

  implicit def expEval = new EVAL[EXP, Double] {
    override def eval(v: EXP[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        exp(v.v(x))
      }
    }
  }

  implicit def logEval = new EVAL[LOG, Double] {
    override def eval(v: LOG[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        log(v.v(x))
      }
    }
  }

  implicit def powEval = new EVAL[POW, Double] {
    override def eval(v: POW[AlgebraFunction[Double]]): AlgebraFunction[Double] = {
      x: Map[String, Variable[Double]] => {
        pow(v.lv(x), v.rv(x))
      }
    }
  }

  implicit def coproductEval[F[_], G[_], A](implicit feval: EVAL[F, A], geval: EVAL[G, A]) = new EVAL[Coproduct[F, G, ?], A] {
    override def eval(v: Coproduct[F, G, AlgebraFunction[A]]): AlgebraFunction[A] = {
      v.run match {
        case l: Left[F[AlgebraFunction[A]], G[AlgebraFunction[A]]] => feval.eval(l.a)
        case r: Right[F[AlgebraFunction[A]], G[AlgebraFunction[A]]] => geval.eval(r.b)
      }
    }
  }

  def eval[F[_]](expr: EXPR[F])(implicit ffunctor: Functor[F],
                                intEval:EVAL[F,Int],
                                doubleEval:EVAL[F,Double],
                                typeInspect: TypeInspect[F]): Map[String, Algebra] => Any= {
        val algebra = inspect(expr)
        algebra match {
          case _:IntAlgebra => {
            val feval:EVAL[F,Int] = implicitly[EVAL[F,Int]]
            val func=EXPR.fold(expr) { v: F[AlgebraFunction[Int]] => feval.eval(v) }
            x:Map[String,Algebra] => {
              val paras=x.mapValues{x:Algebra => Variable(x.toIntV()) }
              func(paras).v
            }
          }
          case _:DoubleAlgebra => {
            val feval:EVAL[F,Double] = implicitly[EVAL[F,Double]]
            val func=EXPR.fold(expr) { v: F[AlgebraFunction[Double]] => feval.eval(v) }
            x:Map[String,Algebra] => {
              val paras=x.mapValues{x:Algebra => Variable(x.toDoubleV()) }
              func(paras).v
            }
          }
        }
  }
}
