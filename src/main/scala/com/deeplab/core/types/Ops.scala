package com.deeplab.core.types

import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * Created by jiaming.shang on 4/14/17.
  */
trait Ops[A] {
  def +(lv:A,rv:A):A
  def -(lv:A,rv:A):A
  def *(lv:A,rv:A):A
  def /(lv:A,rv:A):A
  def exp(cap:A):A
  def pow(lv:A,rv:A):A
  def log(v:A):A
}

object Ops {
  def +[A](lv:A,rv:A):A ={
    throw NotImplementedException
  }
  def -[A](lv:A,rv:A):A ={
    throw NotImplementedException
  }
  def *[A](lv:A,rv:A):A ={
    throw NotImplementedException
  }
  def /[A](lv:A,rv:A):A = {
    throw NotImplementedException
  }
  def exp[A](cap:A):A ={
    throw NotImplementedException
  }
  def pow[A](lv:A,rv:A):A={
    throw NotImplementedException
  }
  def log[A](v:A):A={
    throw NotImplementedException
  }

  def zero[A]():A ={
    throw NotImplementedException
  }

  def one[A]():A={
    throw NotImplementedException
  }
}
