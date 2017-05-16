package com.deeplab.core.types

import cats.data.Coproduct
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import cats.free.Inject
import EXPR._


/**
  * Created by jiaming.shang on 4/14/17.
  */
trait Ops[A] {
  def +(lv: A, rv: A): A

  def -(lv: A, rv: A): A

  def *(lv: A, rv: A): A

  def /(lv: A, rv: A): A
}

class DoubleOps extends Ops[Double] {
  override def +(lv: Double, rv: Double): Double = lv + rv

  override def /(lv: Double, rv: Double): Double = lv / rv

  override def -(lv: Double, rv: Double): Double = lv - rv

  override def *(lv: Double, rv: Double): Double = lv * rv
}

class IntOps extends Ops[Int] {
  override def +(lv: Int, rv: Int): Int = lv + rv

  override def /(lv: Int, rv: Int): Int = lv / rv

  override def -(lv: Int, rv: Int): Int = lv - rv

  override def *(lv: Int, rv: Int): Int = lv * rv

}

object Ops {
  implicit val doubleOps: Ops[Double] = new DoubleOps()
  implicit val intOps: Ops[Int] = new IntOps()

  implicit def intToDouble(v: Int): Double = v toDouble
}
