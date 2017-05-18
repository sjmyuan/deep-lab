package com.deeplab.core.types

import org.scalatest.FunSpec
import Ops._
import Variable._
import EXPR._
import EVAL._
import org.scalatest._

/**
  * Created by jiaming.shang on 4/12/17.
  */
class OPSpec extends FunSpec with Matchers {
  describe("An OP") {

    describe("Expression") {
      describe("Add"){
        describe("left associate"){
          it("should return the right expression") {
            val expr1 = dscalar("x")
            val expr2 = dscalarVal(10.0)
            val result = expr1 + 10.0
            val expected = addExpr(expr1, expr2)

            result.v should be (expected)
          }
        }
        describe("right associate"){
          it("should return the right expression") {
            val expr1 = dscalar("x")
            val expr2 = dscalarVal(10.0)
            val result = 10.0 + expr1
            val expected = addExpr(expr2, expr1)

            result.v should be (expected)
          }
        }
      }
    }

    describe("EVAL"){
      describe("VAL") {
        it("should return the constant value"){
          val expr= dscalarVal(10)
          val realExpr = eval(expr)
          val result = realExpr(Map())
          result should be (10)
        }
      }

      describe("VAR") {
        it("should return the variable value"){
          val expr= dscalar("x")
          val realExpr = eval(expr)
          val result = realExpr(Map("x"->10))
          result should be (10)
        }
      }

      describe("ADD") {
        describe("two var"){
          it("should return the sum variable value"){
            val x= dscalar("x")
            val y= dscalar("y")
            val expr=x+y
            val realExpr = eval(expr)
            val result = realExpr(Map("x"->10,"y"->20))
            result should be (30)
          }
        }

        describe("two val"){
          it("should return the sum const value"){
            val x= dscalarVal(10)
            val y= dscalarVal(20)
            val expr=x+y
            val realExpr = eval(expr)
            val result = realExpr(Map())
            result should be (30)
          }
        }

        describe("var and val"){
          describe("left associate") {
            it("should return the sum of var and val"){
              val x= dscalar("x")
              val expr=x+10.0
              val realExpr = eval(expr)
              val result = realExpr(Map("x"->20.0))
              result should be (30)
            }
          }

          describe("right associate") {
            it("should return the sum of var and val"){
              val x= dscalar("x")
              val expr=10.0 + x
              val realExpr = eval(expr)
              val result = realExpr(Map("x"->20.0))
              result should be (30)
            }
          }
        }
      }
    }
  }
}
