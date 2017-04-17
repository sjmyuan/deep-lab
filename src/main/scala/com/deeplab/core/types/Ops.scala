package com.deeplab.core.types

import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * Created by jiaming.shang on 4/14/17.
  */
trait Ops[A] {
  def +(lv: A, rv: A): A

  def -(lv: A, rv: A): A

  def *(lv: A, rv: A): A

  def /(lv: A, rv: A): A

  def exp(cap: A): A

  def pow(lv: A, rv: A): A

  def log(v: A): A

  def zero(): A

  def one(): A

  def ==(lv: A, rv: A): Boolean
}

class DoubleOps extends Ops[Double] {
  override def +(lv: Double, rv: Double): Double = lv + rv

  override def one(): Double = 1.0

  override def /(lv: Double, rv: Double): Double = lv / rv

  override def log(v: Double): Double = Math.log(v)

  override def exp(cap: Double): Double = Math.exp(cap)

  override def pow(lv: Double, rv: Double): Double = Math.pow(lv, rv)

  override def -(lv: Double, rv: Double): Double = lv - rv

  override def *(lv: Double, rv: Double): Double = lv * rv

  override def zero(): Double = 0.0

  override def ==(lv: Double, rv: Double): Boolean = lv == rv
}

object Ops {
  implicit val doubleOps: Ops[Double] = new DoubleOps()
}
