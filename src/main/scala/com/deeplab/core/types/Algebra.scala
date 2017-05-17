package com.deeplab.core.types

/**
  * Created by jiaming.shang on 5/17/17.
  */

trait Algebra {
  def Index(): Int
  def toDouble(): Algebra
}

class DoubleAlgebra extends Algebra {
  def Index() = 2
  def toDouble()= new DoubleAlgebra()
}

class FloatAlgebra extends Algebra {
  def Index() = 1
  def toDouble() = new DoubleAlgebra()
}

class IntAlgebra extends Algebra {
  def Index() = 0
  def toDouble() = new DoubleAlgebra()
}

