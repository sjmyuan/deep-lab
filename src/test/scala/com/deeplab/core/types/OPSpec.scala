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
        it("should return the right add expression"){
          val expr1= valX(10)
          val expr2= valX(10)
          val result = expr1 + expr2
          val expected = addExpr(expr1,expr2)

          result.v should be (expected)
        }

        it("should return the wrong add expression"){
          val expr1= valX(10)
          val expr2= valX(10)
          val expr3= valX(11)
          val result = expr1 + expr2
          val expected = addExpr(expr1,expr3)

          result.v should not be (expected)
        }
      }
    }

    describe("EVAL"){
      describe("VAL") {
        it("should return the constant value"){
          val expr= valX(10)
          val realExpr = eval(expr)
          val result = realExpr(Map())
          result.v should be (10)
        }
      }

      describe("VAR") {
        it("should return the variable value"){
          val expr= varX("x")
          val realExpr = eval(expr)
          val result = realExpr(Map("x"->10))
          result.v should be (10)
        }
      }

      describe("ADD") {
        describe("two var"){
          it("should return the sum variable value"){
            val x= varX("x")
            val y= varX("y")
            val expr=x+y
            val realExpr = eval(expr)
            val result = realExpr(Map("x"->10,"y"->20))
            result.v should be (30)
          }
        }

        describe("two val"){
          it("should return the sum const value"){
            val x= valX(10)
            val y= valX(20)
            val expr=x+y
            val realExpr = eval(expr)
            val result = realExpr(Map())
            result.v should be (30)
          }
        }
      }
    }
  }
}
