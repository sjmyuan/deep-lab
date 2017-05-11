//package com.deeplab.core.types
//
///**
//  * Created by jiaming.shang on 4/11/17.
//  */
//case class VAL[A](v:Int)
//
//case class VALINT(v:EXPR[VAL])
//
//trait SYMBOL[A];
//
//case class VAR[A](name: String) extends SYMBOL[A]
//
//case class CONS[A](name: String, v: A) extends SYMBOL[A]
//
//case class ADD[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]
//
//case class SUB[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]
//
//case class MUL[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]
//
//case class DIV[A](lv: SYMBOL[A], rv: SYMBOL[A]) extends SYMBOL[A]
//
//case class POW[A](v: SYMBOL[A], cap: SYMBOL[A]) extends SYMBOL[A]
//
//case class EXP[A](cap: SYMBOL[A]) extends SYMBOL[A]
//
//case class LOG[A](v: SYMBOL[A]) extends SYMBOL[A]
//
//object SYMBOL {
//  def eval[A](op: SYMBOL[A])(implicit ops: Ops[A]): Variable[A] => Variable[A] = {
//    op match {
//      case VAR(_) => { (x: Variable[A]) => x }
//      case CONS(_, v) => { (x: Variable[A]) => Variable(v) }
//      case ADD(lv, rv) => { x: Variable[A] => eval(lv)(ops)(x) + eval(rv)(ops)(x) }
//      case SUB(lv, rv) => { x: Variable[A] => eval(lv)(ops)(x) - eval(rv)(ops)(x) }
//      case MUL(lv, rv) => { x: Variable[A] => eval(lv)(ops)(x) * eval(rv)(ops)(x) }
//      case DIV(lv, rv) => { x: Variable[A] => eval(lv)(ops)(x) / eval(rv)(ops)(x) }
//      case POW(v, cap) => { x: Variable[A] => eval(v)(ops)(x) pow eval(cap)(ops)(x) }
//      case EXPR(cap) => { x: Variable[A] => eval(cap)(ops)(x) exp }
//      case LOG(v) => { x: Variable[A] => eval(v)(ops)(x) log }
//    }
//  }
//
//  def grad[A](op: SYMBOL[A], name: String)(implicit ops: Ops[A]): Variable[A] => Variable[A] = {
//    op match {
//      case VAR(vn) => if (vn == name) { x: Variable[A] => Variable(ops.one()) } else { x: Variable[A] => Variable(ops.zero()) }
//      case CONS(vn, v) => if (vn == name) { x: Variable[A] => Variable(ops.one()) } else { x: Variable[A] => Variable(ops.zero()) }
//      case ADD(lv, rv) => { x: Variable[A] => grad(lv, name)(ops)(x) + grad(rv, name)(ops)(x) }
//      case SUB(lv, rv) => { x: Variable[A] => grad(lv, name)(ops)(x) - grad(rv, name)(ops)(x) }
//      case MUL(lv, rv) => {
//        x: Variable[A] =>
//          grad(lv, name)(ops)(x) * eval(rv)(ops)(x) + eval(lv)(ops)(x) * grad(rv, name)(ops)(x)
//      }
//      case DIV(lv, rv) => {
//        x: Variable[A] => {
//          val ldf = grad(lv, name)(ops)(x)
//          val rdf = grad(rv, name)(ops)(x)
//          val lf = eval(lv)(ops)(x)
//          val rf = eval(rv)(ops)(x)
//          val one = Variable(ops.one)
//          ldf / rf - lf / (rf pow (one + one)) * rdf
//        }
//      }
//      case POW(v, cap) => {
//        x: Variable[A] => {
//          val capf = eval(cap)(ops)(x)
//          val vf = eval(v)(ops)(x)
//          val capdf = grad(cap, name)(ops)(x)
//          val vdf = grad(v, name)(ops)(x)
//          capf * (vf pow (capf - Variable(ops.one))) * vdf + (vf pow capf) * vf.log * capdf
//        }
//      }
//      case EXPR(cap) => {
//        x: Variable[A] => {
//          eval(cap)(ops)(x).exp * grad(cap, name)(ops)(x)
//        }
//      }
//      case LOG(v) => {
//        x: Variable[A] => {
//          Variable(ops.one()) / eval(v)(ops)(x) * grad(v, name)(ops)(x)
//        }
//      }
//    }
//  }
//}
