package com.deeplab.core.types
import EXPR._
import cats.Functor
import cats.data.Coproduct
import cats.free.Inject
import com.deeplab.core.types.GRAD.OriginAndGrad
/**
  * Created by jiaming.shang on 5/24/17.
  */
trait GRAD[F[_]] {
  def grad(v: F[OriginAndGrad], x:VAR[_]): OriginAndGrad
}

object GRAD{

  type OriginAndGrad=(EXPR[EXPRTYPE],EXPR[EXPRTYPE])

  implicit def valGrad = new GRAD[VAR] {
    override def grad(v: VAR[OriginAndGrad], x:VAR[_]): OriginAndGrad = {
      if (v.name == x.name)
        (inject[VAR,EXPRTYPE](v.asInstanceOf[VAR[EXPR[EXPRTYPE]]]),dscalarVal(1))
      else
        (inject[VAR,EXPRTYPE](v.asInstanceOf[VAR[EXPR[EXPRTYPE]]]),dscalarVal(0))
    }
  }

  implicit def addGrad = new GRAD[ADD] {
    override def grad(v: ADD[OriginAndGrad], x:VAR[_]): OriginAndGrad = {
      (addExpr(v.lv._1,v.rv._1),addExpr(v.lv._2,v.rv._2))
    }
  }

  implicit def subGrad = new GRAD[SUB] {
    override def grad(v: SUB[OriginAndGrad], x:VAR[_]): OriginAndGrad = {
      (subExpr(v.lv._1,v.rv._1),subExpr(v.lv._2,v.rv._2))
    }
  }

  implicit def mulGrad = new GRAD[MUL] {
    override def grad(v: MUL[OriginAndGrad], x:VAR[_]): OriginAndGrad = {
      val origin = mulExpr(v.lv._1,v.rv._1)
      val diff =
        addExpr(mulExpr(v.lv._2,v.rv._1),mulExpr(v.lv._1,v.rv._2))
      (origin,diff)
    }
  }

  implicit def divGrad = new GRAD[DIV] {
    override def grad(v: DIV[OriginAndGrad], x:VAR[_]): OriginAndGrad = {
      val origin = divExpr(v.lv._1,v.rv._1)
      val diff =
      subExpr(divExpr(v.lv._2,v.rv._1),divExpr(mulExpr(v.lv._1,v.rv._2),addExpr(v.rv._1,v.rv._1)))
      (origin,diff)
    }
  }

  implicit def powGrad = new GRAD[POW] {
    override def grad(v: POW[OriginAndGrad], x: VAR[_]): OriginAndGrad ={
      val origin = powExpr(v.lv._1,v.rv._1)
      val diff =
        addExpr(mulExpr(mulExpr(v.rv._1,powExpr(v.lv._1,subExpr(v.rv._1,iscalarVal(1)))),v.lv._2),mulExpr(mulExpr(origin,logExpr(v.lv._1)),v.rv._2))
      (origin,diff)
    }
  }

  implicit def logGrad = new GRAD[LOG] {
    override def grad(v: LOG[OriginAndGrad], x: VAR[_]): OriginAndGrad ={
      val origin = logExpr(v.v._1)
      val diff = mulExpr(divExpr(iscalarVal(1),v.v._1),v.v._2)
      (origin,diff)
    }
  }

  implicit def expGrad = new GRAD[EXP] {
    override def grad(v: EXP[OriginAndGrad], x: VAR[_]): OriginAndGrad ={
      val origin = expExpr(v.v._1)
      val diff = mulExpr(origin,v.v._2)
      (origin,diff)
    }
  }

  implicit def coproductGrad[F[_], G[_], A](implicit fgrad: GRAD[F], ggrad: GRAD[G]) = new GRAD[Coproduct[F, G, ?]] {
    override def grad(v: Coproduct[F, G, OriginAndGrad], x:VAR[_]): OriginAndGrad= {
      v.run match {
        case l: Left[F[OriginAndGrad], G[OriginAndGrad]] => fgrad.grad(l.a,x)
        case r: Right[F[OriginAndGrad], G[OriginAndGrad]] => ggrad.grad(r.b,x)
      }
    }
  }

  def grad[F[_]:Functor:GRAD](expr:EXPR[F], x:EXPR[EXPRTYPE]):EXPR[EXPRTYPE]={
    val grad= implicitly[GRAD[F]]
    val inject = implicitly[Inject[VAR,EXPRTYPE]]

    inject.prj(x.v).map(varX =>
      EXPR.fold(expr) {v:F[OriginAndGrad] => grad.grad(v,varX)}._2
    ) match {
      case Some(x) => x
      case None => throw new Exception()
    }
  }
}

