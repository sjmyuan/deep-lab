package com.deeplab.core.expression

import cats.free.{Free, Inject}
import com.deeplab.core.types._

/**
  * Created by jiaming.shang on 4/11/17.
  */
class OPI[F[_]](implicit I: Inject[OP, F]) {
  def variable[A](v: A): Free[F, A] = Free.inject[OP, F](VARIABLE(v))

  def add[A](lv: OP[A], rv: OP[A]): Free[F, A] = Free.inject[OP, F](ADD(lv, rv))

  def minus[A](lv: OP[A], rv: OP[A]): Free[F, A] = Free.inject[OP, F](MINUS(lv, rv))

  def times[A](lv: OP[A], rv: OP[A]): Free[F, A] = Free.inject[OP, F](TIMES(lv, rv))

  def divide[A](lv: OP[A], rv: OP[A]): Free[F, A] = Free.inject[OP, F](DIVIDE(lv, rv))
}

object OPI {
  implicit def requestI[F[_]](implicit I: Inject[OP, F]): OPI[F] = new OPI[F]()
}

