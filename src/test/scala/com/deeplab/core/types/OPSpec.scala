package com.deeplab.core.types

import org.scalatest.FunSpec
import Ops._
import Variable._
import EXPR._
import Inject._
/**
  * Created by jiaming.shang on 4/12/17.
  */
class OPSpec extends FunSpec {
  describe("An OP") {
    describe("VAL") {
      it("should return the constant value"){
        val expr:EXPR[EXPRTYPE] = valExpr[EXPRTYPE](10)
        val value = fold[EXPRTYPE,Int](expr){x:EXPRTYPE[Int] => 1}
        assert(value == 10)
      }
    }
//  describe("An OP") {
//    describe("eval[Double]") {
//      describe("when is ADD") {
//        it("should return the sum of two number") {
//          val sum = ADD(CONS("", 1.0), CONS("", 1.0))
//          val f = SYMBOL.eval[Double](sum)
//          assert(f(1.0).v == 2.0)
//        }
//      }
//    }
//    describe("derivate") {
//      describe("when is ADD") {
//        it("should return the sum of derivate") {
////          val sum = ADD(VAR("x"), VAR("x"))
////          val f = SYMBOL.grad[Double](sum, "x")
////          assert(f(1) == 2)
//        }
//      }
//      describe("when is MUL") {
//        it("should return the derivate according to chain rule") {
////          val mul = MUL(VAR("x"), VAR("x"))
////          val f = SYMBOL.grad[Double](mul, "x")
////          assert(f(2) == 4)
//        }
//      }
//      describe("when is DIV") {
//        it("should return the derivate according to chain rule") {
//          val div = DIV(CONS("", 1.0), VAR("x"))
//          val f = SYMBOL.grad[Double](div, "x")
//          assert(f(2) == -0.25)
//        }
//      }
//      describe("when is POW") {
//        describe("when variable in base") {
//          it("should return the derivate according to chain rule") {
//            val pow = POW(VAR("x"), CONS("", 2))
//            val f = SYMBOL.grad[Double](pow, "x")
//            assert(f(3) == 6)
//          }
//        }
//        describe("when variable in cap") {
//          it("should return the derivate according to chain rule") {
//            val pow = POW(CONS("", 2), VAR("x"))
//            val f = SYMBOL.grad[Double](pow, "x")
//            assert(f(3) == 8 * Math.log(2))
//          }
//        }
//      }
//      describe("when is EXP") {
//        it("should return the derivate according to chain rule") {
//          val exp = EXP(VAR("x"))
//          val f = SYMBOL.grad[Double](exp, "x")
//          assert(f(3) == Math.exp(3))
//        }
//      }
//      describe("when is LOG") {
//        it("should return the derivate according to chain rule") {
//          val log = LOG(VAR("x"))
//          val f = SYMBOL.grad[Double](log, "x")
//          assert(f(2) == 0.5)
//        }
//      }
//    }
  }
}
