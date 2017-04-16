package com.deeplab.core.types

/**
  * Created by jiaming.shang on 4/11/17.
  */

trait SYMBOL[A];

case class VAR[A](name: String) extends SYMBOL[A]

case class CONS[A](name: String, v: A) extends SYMBOL[A]

case class ADD[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]

case class SUB[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]

case class MUL[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]

case class DIV[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]

case class POW[A](v: SYMBOL[A], cap: SYMBOL[A]) extends SYMBOL[A]

case class EXP[A](cap: SYMBOL[A]) extends SYMBOL[A]

case class LOG[A](v: SYMBOL[A]) extends SYMBOL[A]

object SYMBOL {
  def eval[A](op: SYMBOL[A]): A => A = {
    op match {
      case VAR(_) => { (x: A) => x }
      case CONS(_, v) => { (x: A) => v }
      case ADD(lv, rv) => { x: A => Ops.+(eval(lv)(x), eval(rv)(x)) }
      case SUB(lv, rv) => { x: A => Ops.-(eval(lv)(x), eval(rv)(x)) }
      case MUL(lv, rv) => { x: A => Ops.*(eval(lv)(x), eval(rv)(x)) }
      case DIV(lv, rv) => { x: A => Ops./(eval(lv)(x), eval(rv)(x)) }
      case POW(v, cap) => { x: A => Ops.pow(eval(v)(x), eval(cap)(x)) }
      case EXP(cap) => { x: A => Ops.exp(eval(cap)(x)) }
      case LOG(v) => { x: A => Ops.log(eval(v)(x)) }
    }
  }

  def grad[A](op: SYMBOL[A], name: String): A => A = {
    op match {
      case VAR(vn) => if (vn == name) { x: A => Ops.one() } else { x: A => Ops.zero() }
      case CONS(vn, v) => if (vn == name) { x: A => Ops.one() } else { x: A => Ops.zero() }
      case ADD(lv, rv) => { x: A => Ops.+(grad(lv, name)(x), grad(rv, name)(x)) }
      case SUB(lv, rv) => { x: A => Ops.-(grad(lv, name)(x), grad(rv, name)(x)) }
      case MUL(lv, rv) => {
        x: A =>
          Ops.+(Ops.*(grad(lv, name)(x), eval(rv)(x)), Ops.*(eval(lv)(x), grad(rv, name)(x)))
      }
      case DIV(lv, rv) => {
        x: A => {
          val ldf = grad(lv, name)(x)
          val rdf = grad(rv, name)(x)
          val lf = eval(lv)(x)
          val rf = eval(rv)(x)
          Ops.-(Ops./(ldf, rf), Ops./(lf, Ops.*(Ops.pow(rf, Ops.+(Ops.one(), Ops.one())), rdf)))
        }
      }
      case POW(v, cap) => {
        x: A => {
          val capf = eval(cap)(x)
          val vf = eval(v)(x)
          val capdf = grad(cap, name)(x)
          val vdf = grad(v, name)(x)
          Ops.+(Ops.*(Ops.*(capf, Ops.pow(vf, Ops.-(capf, Ops.one))), vdf), Ops.*(Ops.*(Ops.pow(vf, capf), Ops.log(vf)), capdf))
        }
      }
      case EXP(cap) => {
        x: A => {
          Ops.*(Ops.exp(eval(cap)(x)), grad(cap, name)(x))
        }
      }
      case LOG(v) => {
        x: A => {
          Ops.*(Ops./(Ops.one(), eval(v)(x)), grad(v, name)(x))
        }
      }
    }
  }
}
