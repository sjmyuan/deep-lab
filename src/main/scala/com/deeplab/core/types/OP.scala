package com.deeplab.core.types

/**
  * Created by jiaming.shang on 4/11/17.
  */

trait OP[A];

case class VARIABLE[A](v: A) extends OP[A]

case class ADD[A](lv: OP[A], rv: OP[A]) extends OP[A]

case class MINUS[A](lv: OP[A], rv: OP[A]) extends OP[A]

case class TIMES[A](lv: OP[A], rv: OP[A]) extends OP[A]

case class DIVIDE[A](lv: OP[A], rv: OP[A]) extends OP[A]
