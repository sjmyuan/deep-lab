package com.deeplab.core.types

import cats.free.Inject
import EXPR._

/**
  * Created by jiaming.shang on 5/7/17.
  */
sealed case class EXPRWrapper[F[_]](v: EXPR[F]) {
  def +(o: EXPRWrapper[F])(implicit I: Inject[ADD, F], varI: Inject[VAR, F]): EXPRWrapper[F] = {
    EXPRWrapper(addExpr(v, o.v))
  }

  def +[A](o: A)(implicit map: A => EXPRWrapper[F], I: Inject[ADD, F], varI: Inject[VAR, F]): EXPRWrapper[F] = {
    EXPRWrapper(addExpr(v, o.v))
  }

  def -(o: EXPRWrapper[F])(implicit I: Inject[SUB, F], varI: Inject[VAR, F], negI: Inject[NEG, F]): EXPRWrapper[F] = {
    EXPRWrapper(subExpr(v, o.v))
  }

  def *(o: EXPRWrapper[F])(implicit I: Inject[MUL, F], varI: Inject[VAR, F]): EXPRWrapper[F] = {
    EXPRWrapper(mulExpr(v, o.v))
  }

  def *[A](o: A)(implicit map: A => EXPRWrapper[F], I: Inject[MUL, F], varI: Inject[VAR, F]): EXPRWrapper[F] = {
    EXPRWrapper(mulExpr(v, o.v))
  }

  def /(o: EXPRWrapper[F])(implicit I: Inject[DIV, F], varI: Inject[VAR, F]): EXPRWrapper[F] = {
    EXPRWrapper(divExpr(v, o.v))
  }

  def /[A](o: A)(implicit map: A => EXPRWrapper[F], I: Inject[DIV, F], varI: Inject[VAR, F]): EXPRWrapper[F] = {
    EXPRWrapper(divExpr(v, o.v))
  }

  def exp()(implicit I: Inject[EXP, F]): EXPRWrapper[F] = {
    EXPRWrapper(expExpr(v))
  }

  def pow(cap: EXPRWrapper[F])(implicit I: Inject[POW, F]): EXPRWrapper[F] = {
    EXPRWrapper(powExpr(v, cap.v))
  }

  def log()(implicit I: Inject[LOG, F]): EXPRWrapper[F] = {
    EXPRWrapper(logExpr(v))
  }

  def unary_-(implicit I: Inject[NEG, F],varI:Inject[VAR,F]): EXPRWrapper[F] = {
    EXPRWrapper(negExpr(v))
  }
}
