package com.deeplab.core.types

/**
  * Created by jiaming.shang on 4/11/17.
  */

trait OP;

case class VAR(name: String, shape: (Int, Int)) extends OP

case class CONS(name: String, shape: (Int, Int), v: Double) extends OP

case class ADD(lv: OP, rv: OP) extends OP

case class SUB(lv: OP, rv: OP) extends OP

case class MUL(lv: OP, rv: OP) extends OP

case class DIV(lv: OP, rv: OP) extends OP

case class POW(v: OP, cap: OP) extends OP

case class EXP(cap: OP) extends OP

case class LOG(v: OP) extends OP

object OP {
  def eval(op: OP): Double => Double = {
    op match {
      case VAR(_, _) => { (x: Double) => x }
      case CONS(_, _, v) => { (x: Double) => v }
      case ADD(lv, rv) => { x: Double => eval(lv)(x) + eval(rv)(x) }
      case SUB(lv, rv) => { x: Double => eval(lv)(x) - eval(rv)(x) }
      case MUL(lv, rv) => { x: Double => eval(lv)(x) * eval(rv)(x) }
      case DIV(lv, rv) => { x: Double => eval(lv)(x) / eval(rv)(x) }
      case POW(v, cap) => { x: Double => Math.pow(eval(v)(x), eval(cap)(x)) }
      case EXP(cap) => { x: Double => Math.exp(eval(cap)(x)) }
      case LOG(v) => { x: Double => Math.log(eval(v)(x)) }
    }
  }

  def grad(op: OP, name: String): Double => Double = {
    op match {
      case VAR(vn, _) => if (vn == name) { x: Double => 1.0 } else { x: Double => 0 }
      case CONS(vn, _, v) => if (vn == name) { x: Double => 1.0 } else { x: Double => 0 }
      case ADD(lv, rv) => { x: Double => grad(lv, name)(x) + grad(rv, name)(x) }
      case SUB(lv, rv) => { x: Double => grad(lv, name)(x) - grad(rv, name)(x) }
      case MUL(lv, rv) => {
        x: Double =>
          grad(lv, name)(x) * eval(rv)(x) + eval(lv)(x) * grad(rv, name)(x)
      }
      case DIV(lv, rv) => {
        x: Double => {
          val ldf = grad(lv, name)(x)
          val rdf = grad(rv, name)(x)
          val lf = eval(lv)(x)
          val rf = eval(rv)(x)
          ldf / rf - lf / Math.pow(rf, 2)*rdf
        }
      }
      case POW(v, cap) => {
        x: Double => {
          val capf = eval(cap)(x)
          val vf = eval(v)(x)
          val capdf = grad(cap, name)(x)
          val vdf = grad(v, name)(x)
          capf * Math.pow(vf, capf - 1) * vdf + Math.pow(vf, capf) * Math.log(vf) * capdf
        }
      }
      case EXP(cap) => {
        x: Double => {
          Math.exp(eval(cap)(x)) * grad(cap, name)(x)
        }
      }
      case LOG(v) => {
        x: Double => {
          1 / eval(v)(x) * grad(v, name)(x)
        }
      }
    }
  }
}
