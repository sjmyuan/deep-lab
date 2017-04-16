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
  def eval[A](op: SYMBOL[A])(implicit ops: Ops[A]): A => A = {
    op match {
      case VAR(_) => { (x: A) => x }
      case CONS(_, v) => { (x: A) => v }
      case ADD(lv, rv) => { x: A => ops.+(eval(lv)(ops)(x), eval(rv)(ops)(x)) }
      case SUB(lv, rv) => { x: A => ops.-(eval(lv)(ops)(x), eval(rv)(ops)(x)) }
      case MUL(lv, rv) => { x: A => ops.*(eval(lv)(ops)(x), eval(rv)(ops)(x)) }
      case DIV(lv, rv) => { x: A => ops./(eval(lv)(ops)(x), eval(rv)(ops)(x)) }
      case POW(v, cap) => { x: A => ops.pow(eval(v)(ops)(x), eval(cap)(ops)(x)) }
      case EXP(cap) => { x: A => ops.exp(eval(cap)(ops)(x)) }
      case LOG(v) => { x: A => ops.log(eval(v)(ops)(x)) }
    }
  }

  def grad[A](op: SYMBOL[A], name: String)(implicit ops: Ops[A]): A => A = {
    op match {
      case VAR(vn) => if (vn == name) { x: A => ops.one() } else { x: A => ops.zero() }
      case CONS(vn, v) => if (vn == name) { x: A => ops.one() } else { x: A => ops.zero() }
      case ADD(lv, rv) => { x: A => ops.+(grad(lv, name)(ops)(x), grad(rv, name)(ops)(x)) }
      case SUB(lv, rv) => { x: A => ops.-(grad(lv, name)(ops)(x), grad(rv, name)(ops)(x)) }
      case MUL(lv, rv) => {
        x: A =>
          ops.+(ops.*(grad(lv, name)(ops)(x), eval(rv)(ops)(x)), ops.*(eval(lv)(ops)(x), grad(rv, name)(ops)(x)))
      }
      case DIV(lv, rv) => {
        x: A => {
          val ldf = grad(lv, name)(ops)(x)
          val rdf = grad(rv, name)(ops)(x)
          val lf = eval(lv)(ops)(x)
          val rf = eval(rv)(ops)(x)
          ops.-(ops./(ldf, rf), ops./(lf, ops.*(ops.pow(rf, ops.+(ops.one(), ops.one())), rdf)))
        }
      }
      case POW(v, cap) => {
        x: A => {
          val capf = eval(cap)(ops)(x)
          val vf = eval(v)(ops)(x)
          val capdf = grad(cap, name)(ops)(x)
          val vdf = grad(v, name)(ops)(x)
          ops.+(ops.*(ops.*(capf, ops.pow(vf, ops.-(capf, ops.one))), vdf), ops.*(ops.*(ops.pow(vf, capf), ops.log(vf)), capdf))
        }
      }
      case EXP(cap) => {
        x: A => {
          ops.*(ops.exp(eval(cap)(ops)(x)), grad(cap, name)(ops)(x))
        }
      }
      case LOG(v) => {
        x: A => {
          ops.*(ops./(ops.one(), eval(v)(ops)(x)), grad(v, name)(ops)(x))
        }
      }
    }
  }
}
