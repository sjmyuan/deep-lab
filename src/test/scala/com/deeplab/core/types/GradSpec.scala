package com.deeplab.core.types

import org.scalatest._
import EXPR._
import Optimize._
import GRAD._
import EVAL._
import Variable._

/**
  * Created by jiaming.shang on 5/24/17.
  */
class GradSpec extends FunSpec with Matchers {
  describe("Grad") {
    describe("Constant") {
      describe("with same name") {
        it("should return constant 1") {
          val const = dscalarVal(10.0,"x")
          val result = grad(const,const)
          val expected = dscalarVal(1)

          result should be (expected)
        }
      }

      describe("with different name") {
        it("should return constant 0") {
          val const = dscalarVal(10.0,"x")
          val y = dscalar("y")
          val result = grad(const,y)
          val expected = dscalarVal(0)

          result should be (expected)
        }
      }
    }

    describe("Variable") {
      describe("with same name") {
        it("should return constant 1") {
          val const = dscalar("x")
          val result = grad(const,const)
          val expected = dscalarVal(1)

          result should be (expected)
        }
      }

      describe("with different name") {
        it("should return constant 0") {
          val x = dscalar("x")
          val y = dscalar("y")
          val result = grad(x,y)
          val expected = dscalarVal(0)

          result should be (expected)
        }
      }
    }

    describe("ADD") {
      describe("when constant add variable") {
        it("should return 0 add 1"){
          val x = dscalar("x")
          val expr = 1+x
          val result = grad(expr,x)
          val expected = optimize(addExpr(dscalarVal(0),dscalarVal(1)))

          result should be (expected)
        }
      }

      describe("when constant add constant") {
        it("should return 0 "){
          val x = dscalarVal(1)
          val expr = 1+x
          val y = dscalar("y")
          val result = grad(expr,y)
          val expected = dscalarVal(0)

          result should be (expected)
        }
      }
    }

    describe("MUL") {
      describe("when constant times variable") {
        it("should return constant"){
          val x = dscalar("x")
          val expr = 10*x
          val result = grad(expr,x)
          val expected = iscalarVal(10)

          result should be (expected)
        }
      }

      describe("when constant times constant") {
        it("should return 0 "){
          val x = dscalarVal(1)
          val expr = 10*x
          val y = dscalar("y")
          val result = grad(expr,y)
          val expected = dscalarVal(0)

          result should be (expected)
        }
      }

      describe("when x*x") {
        it("should return x+x"){
          val x = dscalar("x")
          val expr = x*x
          val result = grad(expr,x)
          val expected = optimize(addExpr(x,x))

          result should be (expected)
        }
      }
    }

    describe("DIV") {
      describe("when 1/x") {
        it("should return -1/(x+x)"){
          val x = dscalar("x")
          val expr = 1/x
          val result = grad(expr,x)
          val expected:EXPR[EXPRTYPE] = optimize(-(1/(x+x)))

          result should be (expected)
        }
      }
    }

    describe("LOG") {
      describe("when log(x)") {
        it("should return 1/x"){
          val x = dscalar("x")
          val expr = x log
          val result = grad(expr,x)
          val expected:EXPR[EXPRTYPE] = 1/x

          result should be (expected)
        }
      }
    }

    describe("EXP") {
      describe("when exp(x)") {
        it("should return exp(x)"){
          val x = dscalar("x")
          val expr = x exp
          val result = grad(expr,x)
          val expected:EXPR[EXPRTYPE] = x exp

          result should be (expected)
        }
      }
    }

//    describe("POW") {
//      describe("when x^2") {
//        it("should return 2*x"){
//          val x = dscalar("x")
//          val expr = x pow 2
//          val result = grad(expr,x)
//          val expected:EXPR[EXPRTYPE] = 2*x
//
//          result should be (expected)
//        }
//      }
//    }
  }
}
