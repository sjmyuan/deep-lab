package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct

/**
  * Created by jiaming.shang on 5/25/17.
  */
trait PrettyPrint[F[_]] {
  def print(v: F[String]): String
}

object PrettyPrint {

  implicit def varPrint = new PrettyPrint[VAR] {
    override def print(v: VAR[String]): String = {
      v match {
        case x: INTVAL[String] => x.v.v.toString
        case x: INTVAR[String] => x.name
        case x: FLOATVAL[String] => x.v.v.toString
        case x: FLOATVAR[String] => x.name
        case x: DOUBLEVAL[String] => x.v.v.toString
        case x: DOUBLEVAR[String] => x.name
        case x: ZERO[String] => "0"
        case x: ONE[String] => "1"
      }
    }
  }

  implicit def addPrint = new PrettyPrint[ADD] {
    override def print(v: ADD[String]): String = {
      s"(${v.lv}+${v.rv})"
    }
  }

  implicit def subPrint = new PrettyPrint[SUB] {
    override def print(v: SUB[String]): String = {
      s"(${v.lv}-${v.rv})"
    }
  }

  implicit def mulPrint = new PrettyPrint[MUL] {
    override def print(v: MUL[String]): String = {
      s"(${v.lv}*${v.rv})"
    }
  }

  implicit def divPrint = new PrettyPrint[DIV] {
    override def print(v: DIV[String]): String = {
      s"(${v.lv}/${v.rv})"
    }
  }

  implicit def powPrint = new PrettyPrint[POW] {
    override def print(v: POW[String]): String = {
      s"(${v.lv}**${v.rv})"
    }
  }

  implicit def logPrint = new PrettyPrint[LOG] {
    override def print(v: LOG[String]): String = {
      s"log(${v.v})"
    }
  }

  implicit def expPrint = new PrettyPrint[EXP] {
    override def print(v: EXP[String]): String = {
      s"exp(${v.v})"
    }
  }

  implicit def negPrint = new PrettyPrint[NEG] {
    override def print(v: NEG[String]): String = {
      s"-(${v.v})"
    }
  }

  implicit def coproductPrint[F[_], G[_], A](implicit f: PrettyPrint[F], g: PrettyPrint[G]) = new PrettyPrint[Coproduct[F, G, ?]] {
    override def print(v: Coproduct[F, G, String]): String= {
      v.run match {
        case l: Left[F[String], G[String]] => f.print(l.a)
        case r: Right[F[String], G[String]] => g.print(r.b)
      }
    }
  }

  def prettyPrint[F[_]](expr:EXPR[F])(implicit f:Functor[F],p:PrettyPrint[F]):String ={
    EXPR.fold(expr){v:F[String]=>p.print(v)}
  }

}
