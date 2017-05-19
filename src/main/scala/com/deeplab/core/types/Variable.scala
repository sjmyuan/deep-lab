package com.deeplab.core.types

/**
  * Created by jiaming.shang on 4/14/17.
  */

sealed case class Variable[A](v: A)(implicit ops: Ops[A]) {
  def +(o: Variable[A]): Variable[A] = {
    Variable(ops.+(v, o.v))
  }

  def -(o: Variable[A]): Variable[A] = {
    Variable(ops.-(v, o.v))
  }

  def *(o: Variable[A]): Variable[A] = {
    Variable(ops.*(v, o.v))
  }

  def /(o: Variable[A]): Variable[A] = {
    Variable(ops./(v, o.v))
  }
}

object Variable {
  implicit def AtoVariable[A,B](v: B)(implicit t:B=>A, ops: Ops[A]): Variable[A] = {
    Variable(v)
  }

  implicit def intToAlgebra(v:Int):Algebra = IntAlgebra(v)
  implicit def doubleToAlgebra(v:Double):Algebra = DoubleAlgebra(v)

  def exp(v: Variable[Double])= Variable(Math.exp(v.v))
  def log(v: Variable[Double])= Variable(Math.log(v.v))
  def pow(lv: Variable[Double],rv:Variable[Double])= Variable(Math.pow(lv.v,rv.v))

  def expFloat(v: Variable[Float])= Variable[Float](Math.exp(v.v) toFloat)
  def logFloat(v: Variable[Float])= Variable[Float](Math.log(v.v) toFloat)
  def powFloat(lv: Variable[Float],rv:Variable[Float])= Variable[Float](Math.pow(lv.v,rv.v) toFloat)
}
