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
        it("return Int type"){
          val x= iscalarVal(10)
          val y= iscalarVal(20)
          val expr=x+y
          val resultType = inspect(expr)
          resultType.isInstanceOf[IntAlgebra] should be (true)
        }
      }
    }
  }

}
