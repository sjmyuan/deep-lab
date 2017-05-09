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

  def exp(): Variable[A] = {
    Variable(ops.exp(v))
  }

  def pow(cap: Variable[A]): Variable[A] = {
    Variable(ops.pow(v, cap.v))
  }

  def log(): Variable[A] = {
    Variable(ops.log(v))
  }

  def ==(o: Variable[A]): Boolean = {
    ops.==(v, o.v)
  }
}

object Variable {
  implicit def AtoVariable[A,B](v: B)(implicit t:B=>A, ops: Ops[A]): Variable[A] = {
    Variable(v)
  }
}
