package com.deeplab.core.types

import org.scalatest.FunSpec
import org.scalatest._
import EXPR._
import TypeInspect._
/**
  * Created by jiaming.shang on 5/17/17.
  */
class TypeInspectSpec extends FunSpec with Matchers {
  describe("Expression type inspect"){
    describe("ADD") {
      describe("two int constant") {
        it("should return Int type"){
          val x= iscalarVal(10)
          val y= iscalarVal(20)
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [IntAlgebra]
        }
      }

      describe("two double constant") {
        it("should return Double type"){
          val x= dscalarVal(10)
          val y= dscalarVal(20)
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("double add int constant") {
        it("should return Double type"){
          val x= dscalarVal(10)
          val y= iscalarVal(20)
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("int add double constant") {
        it("should return Double type"){
          val x= iscalarVal(10)
          val y= dscalarVal(20)
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("two int variable") {
        it("should return int type"){
          val x= iscalar("x")
          val y= iscalar("y")
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [IntAlgebra]
        }
      }

      describe("two double variable") {
        it("should return double type"){
          val x= dscalar("x")
          val y= dscalar("y")
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("double variable add int variable") {
        it("should return double type"){
          val x= dscalar("x")
          val y= iscalar("y")
          val expr=x+y
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }
    }
    describe("EXP") {
      describe("int constant") {
        it("should return double type") {
          val x= iscalarVal(10)
          val expr= x exp
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("int variable") {
        it("should return double type") {
          val x= iscalar("x")
          val expr= x exp
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("double constant") {
        it("should return double type") {
          val x= dscalarVal(10)
          val expr= x exp
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }

      describe("double variable") {
        it("should return double type") {
          val x= dscalar("x")
          val expr= x exp
          val resultType = inspect(expr)
          resultType shouldBe a [DoubleAlgebra]
        }
      }
    }
  }

}
