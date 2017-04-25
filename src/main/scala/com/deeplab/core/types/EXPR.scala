package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct
import cats.free.Inject

/**
  * Created by jiaming.shang on 4/24/17.
  */
case class EXPR[F[_]](v: F[EXPR[F]])

case class VAL[A](v: Int)

case class ADD[A](lv: A, rv: A)

case class SUB[A](lv: A, rv: A)

case class MUL[A](lv: A, rv: A)

case class DIV[A](lv: A, rv: A)

case class POW[A](lv: A, rv: A)

case class LOG[A](v: A)

case class EXP[A](v: A)

object EXPR {
//  type EXPRTYPE[A] = Coproduct[VAL, Coproduct[ADD, SUB, ?], A]
  type EXPRTYPE[A] = Coproduct[ADD, VAL, A]

  implicit def valFunctor = new Functor[VAL] {
    override def map[A, B](fa: VAL[A])(f: (A) => B): VAL[B] = {
      VAL[B](fa.v)
    }
  }

  implicit def addFunctor = new Functor[ADD] {
    override def map[A, B](fa: ADD[A])(f: (A) => B): ADD[B] = {
      ADD[B](f(fa.lv), f(fa.rv))
    }
  }

  implicit def subFunctor = new Functor[SUB] {
    override def map[A, B](fa: SUB[A])(f: (A) => B): SUB[B] = {
      SUB[B](f(fa.lv), f(fa.rv))
    }
  }

  implicit def mulFunctor = new Functor[MUL] {
    override def map[A, B](fa: MUL[A])(f: (A) => B): MUL[B] = {
      MUL[B](f(fa.lv), f(fa.rv))
    }
  }

  implicit def divFunctor = new Functor[DIV] {
    override def map[A, B](fa: DIV[A])(f: (A) => B): DIV[B] = {
      DIV[B](f(fa.lv), f(fa.rv))
    }
  }

  implicit def powFunctor = new Functor[POW] {
    override def map[A, B](fa: POW[A])(f: (A) => B): POW[B] = {
      POW[B](f(fa.lv), f(fa.rv))
    }
  }

  implicit def logFunctor = new Functor[LOG] {
    override def map[A, B](fa: LOG[A])(f: (A) => B): LOG[B] = {
      LOG[B](f(fa.v))
    }
  }

  implicit def expFunctor = new Functor[EXP] {
    override def map[A, B](fa: EXP[A])(f: (A) => B): EXP[B] = {
      EXP[B](f(fa.v))
    }
  }

  def inject[G[_], F[_]](v: G[EXPR[F]])(implicit I: Inject[G, F]): EXPR[F] = {
    EXPR[F](I.inj(v))
  }

  def valExpr[F[_]](v: Int)(implicit I: Inject[VAL, F]): EXPR[F] = {
    inject[VAL, F](VAL(v))
  }

  def addExpr[F[_]](lv: EXPR[F], rv: EXPR[F])(implicit I: Inject[ADD, F]): EXPR[F] = {
    inject[ADD, F](ADD(lv, rv))
  }

  def subExpr[F[_]](lv: EXPR[F], rv: EXPR[F])(implicit I: Inject[SUB, F]): EXPR[F] = {
    inject[SUB, F](SUB(lv, rv))
  }

  def mulExpr[F[_]](lv: EXPR[F], rv: EXPR[F])(implicit I: Inject[MUL, F]): EXPR[F] = {
    inject[MUL, F](MUL(lv, rv))
  }

  def divExpr[F[_]](lv: EXPR[F], rv: EXPR[F])(implicit I: Inject[DIV, F]): EXPR[F] = {
    inject[DIV, F](DIV(lv, rv))
  }

  def powExpr[F[_]](lv: EXPR[F], rv: EXPR[F])(implicit I: Inject[POW, F]): EXPR[F] = {
    inject[POW, F](POW(lv, rv))
  }

  def logExpr[F[_]](v: EXPR[F])(implicit I: Inject[LOG, F]): EXPR[F] = {
    inject[LOG, F](LOG(v))
  }

  def expExpr[F[_]](v: EXPR[F])(implicit I: Inject[EXP, F]): EXPR[F] = {
    inject[EXP, F](EXP(v))
  }

  def fold[F[_] : Functor, A](fa: EXPR[F])(f: F[A] => A): A = {
    val functorA = implicitly[Functor[F]]
    f(functorA.map(fa.v) { x => fold(x)(f) })
  }
}

