package com.deeplab.core.types

/**
  * Created by jiaming.shang on 5/17/17.
  */

trait Algebra {
  def Index(): Int

  def toDouble(): Algebra

  def toFloat(): Algebra

  def toInt(): Algebra

  def toDoubleV(): Double

  def toFloatV(): Float

  def toIntV(): Int
}

case class DoubleAlgebra(v: Double = 0) extends Algebra {
  override def Index() = 2

  override def toDouble(): Algebra = this.copy()

  override def toFloat(): Algebra = FloatAlgebra(v toFloat)

  override def toInt(): Algebra = IntAlgebra(v toInt)

  override def toDoubleV(): Double = v toDouble

  override def toFloatV(): Float = v toFloat

  override def toIntV(): Int = v toInt
}

case class FloatAlgebra(v: Float = 0) extends Algebra {
  override def Index() = 1

  override def toDouble() = DoubleAlgebra(v)

  override def toFloat() = this.copy()

  override def toInt() = IntAlgebra(v toInt)

  override def toDoubleV(): Double = v toDouble

  override def toFloatV(): Float = v toFloat

  override def toIntV(): Int = v toInt
}

case class IntAlgebra(v: Int = 0) extends Algebra {
  override def Index() = 0

  override def toDouble() = new DoubleAlgebra()

  override def toFloat() = new DoubleAlgebra()

  override def toInt() = new DoubleAlgebra()

  override def toDoubleV(): Double = v toDouble

  override def toFloatV(): Float = v toFloat

  override def toIntV(): Int = v toInt
}

