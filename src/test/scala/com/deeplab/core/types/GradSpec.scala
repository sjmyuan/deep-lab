package com.deeplab.core.types

import org.scalatest._
import EXPR._
import GRAD._

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
          val expected = addExpr(dscalarVal(0),dscalarVal(1))

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
  }
}
