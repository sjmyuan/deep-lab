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

  def exp(v: Variable[Double])= Variable(Math.exp(v.v))
  def log(v: Variable[Double])= Variable(Math.log(v.v))
  def pow(lv: Variable[Double],rv:Variable[Double])= Variable(Math.pow(lv.v,rv.v))
}
