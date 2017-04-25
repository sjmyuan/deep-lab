package com.deeplab.core.types

import cats.Functor
import cats.data.Coproduct
import cats.data.Coproduct._

/**
  * Created by jiaming.shang on 4/25/17.
  */
trait Inject[F[_], G[_]] {
  def inj[A](sub: F[A]): G[A]
}

object Inject {
  implicit def reflexiveInject[F[_] : Functor]: Inject[F, F] = new Inject[F, F] {
    def inj[A](sub: F[A]): F[A] = sub
  }

  implicit def leftKnownInject[F[_] : Functor, G[_] : Functor]: Inject[F, Coproduct[F, G, ?]] = new Inject[F, Coproduct[F, G, ?]] {
    def inj[A](sub: F[A]): Coproduct[F, G, A] = leftc[F, G, A](sub)
  }

  implicit def leftKnown3Inject[F[_] : Functor, G[_] : Functor, H[_] : Functor](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, ?]] = new Inject[F, Coproduct[H, G, ?]] {
    def inj[A](sub: F[A]): Coproduct[H, G, A] = rightc[H, G, A](I.inj(sub))
  }
}

