/*
Copyright 2012 Twitter, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package com.twitter.bijection

import scala.collection.generic.CanBuildFrom
import scala.util.{Success, Try}

trait CollectionInjectionsVersionSpecific extends StringInjections {

  // This is useful for defining injections, but is too general to be implicit
  def toContainer[A, B, C <: TraversableOnce[A], D <: TraversableOnce[B]](
      goodInv: (D, C) => Boolean
  )(
      implicit inj: Injection[A, B],
      cd: CanBuildFrom[Nothing, B, D],
      dc: CanBuildFrom[Nothing, A, C]
  ): Injection[C, D] =
    new AbstractInjection[C, D] {
      def apply(c: C): D = {
        val builder = cd()
        c foreach { builder += inj(_) }
        builder.result()
      }
      override def invert(d: D): Try[C] = {
        val builder = dc()
        d foreach { b =>
          val thisB = inj.invert(b)
          if (thisB.isSuccess) {
            builder += thisB.get
          } else {
            return InversionFailure.failedAttempt(d)
          }
        }
        val res = builder.result()
        if (goodInv(d, res)) Success(res) else InversionFailure.failedAttempt(d)
      }
    }
}
