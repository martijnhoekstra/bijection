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

/**
  * Bijection for joining together iterables of strings into a single string
  * and splitting them back out. Useful for storing sequences of strings
  * in Config maps.
  */
trait StringJoinBijectionVersionSpecific {
  self: StringJoinBijection.type =>

  /**
    * Convert a collection of numbers to and from a string
    * It's common to have types which we know have at least 1 character in their string
    * representation. Knowing that the empty string is not allowed we can map that to the empty
    * collection:
    * TODO add a Tag appoach to Say that N has no zero-length representations
    */
  def nonEmptyValues[N, B <: TraversableOnce[N]](
      separator: String = DEFAULT_SEP
  )(
      implicit bij: ImplicitBijection[N, String],
      ab: CanBuildFrom[Nothing, N, B]
  ): Bijection[B, String] =
    Bijection
      .toContainer[N, String, B, Iterable[String]]
      .andThen(apply(separator))
      .andThen(Bijection.filterDefault("").inverse)

  /**
    * Converts between any collection of A and and Option[String],
    * given an implicit Bijection[A,String]. To get the final string out,
    * compose with the getOrElse bijection if there is no zero length valid A
    *
    * viaContainer[Int,Set[Int]] andThen Bijection.getOrElse(""): Bijection[Set[Int],String]
    *
    * Note that this can be dangerous with empty collections,
    * as Bijection will try to convert "" -> Int. It's safer to use
    * an instance of type A with the "as" notation for a default item
    * in the collection:
    *
    * viaContainer[Int,Set[Int]] andThen Bijection.getOrElse(0.as[String]): Bijection[Set[Int],String]
    */
  def viaContainer[A, B <: TraversableOnce[A]](
      separator: String = DEFAULT_SEP
  )(
      implicit bij: Bijection[A, String],
      ab: CanBuildFrom[Nothing, A, B]
  ): Bijection[B, Option[String]] =
    Bijection.toContainer[A, String, B, Iterable[String]] andThen apply(separator)
}
