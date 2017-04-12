package com.deeplab.core.types

import org.scalatest.FunSpec

/**
  * Created by jiaming.shang on 4/12/17.
  */
class OPSpec extends FunSpec {
  describe("An OP") {
    describe("eval") {
      describe("when is ADD") {
        it("should return the sum of two number") {
          val sum = ADD(CONS("", (1, 1), 1), CONS("", (1, 1), 1))
          val f = OP.eval(sum)
          assert(f(1) == 2)
        }
      }
    }
    describe("derivate") {
      describe("when is ADD") {
        it("should return the sum of derivate") {
          val sum = ADD(VAR("x", (1, 1)), VAR("x", (1, 1)))
          val f = OP.grad(sum, "x")
          assert(f(1) == 2)
        }
      }
      describe("when is MUL") {
        it("should return the derivate according to chain rule") {
          val mul = MUL(VAR("x", (1, 1)), VAR("x", (1, 1)))
          val f = OP.grad(mul, "x")
          assert(f(2) == 4)
        }
      }
      describe("when is DIV") {
        it("should return the derivate according to chain rule") {
          val div = DIV(CONS("", (1, 1), 1), VAR("x", (1, 1)))
          val f = OP.grad(div, "x")
          assert(f(2) == -0.25)
        }
      }
      describe("when is POW") {
        describe("when variable in base") {
          it("should return the derivate according to chain rule") {
            val pow = POW(VAR("x", (1, 1)), CONS("", (1, 1), 2))
            val f = OP.grad(pow, "x")
            assert(f(3) == 6)
          }
        }
        describe("when variable in cap") {
          it("should return the derivate according to chain rule") {
            val pow = POW(CONS("", (1, 1), 2), VAR("x", (1, 1)))
            val f = OP.grad(pow, "x")
            assert(f(3) == 8 * Math.log(2))
          }
        }
      }
      describe("when is EXP") {
        it("should return the derivate according to chain rule") {
          val exp = EXP(VAR("x", (1, 1)))
          val f = OP.grad(exp, "x")
          assert(f(3) == Math.exp(3))
        }
      }
      describe("when is LOG") {
        it("should return the derivate according to chain rule") {
          val log = LOG(VAR("x", (1, 1)))
          val f = OP.grad(log, "x")
          assert(f(2) == 0.5)
        }
      }
    }
  }
}
