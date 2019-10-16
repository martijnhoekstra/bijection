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

import scala.annotation.tailrec

trait StringBijections extends NumericBijections {
  implicit val symbol2String: Bijection[Symbol, String] =
    new AbstractBijection[Symbol, String] {
      def apply(s: Symbol) = s.name
      override def invert(s: String) = Symbol(s)
    }
}

/**
  * Bijection for joining together iterables of strings into a single string
  * and splitting them back out. Useful for storing sequences of strings
  * in Config maps.
  */
object StringJoinBijection extends StringJoinBijectionVersionSpecific {
  val DEFAULT_SEP = ":"

  @tailrec
  private[bijection] def split(str: String, sep: String, acc: List[String] = Nil): List[String] = {
    str.indexOf(sep) match {
      case -1 => (str :: acc).reverse
      case idx: Int =>
        split(str.substring(idx + sep.size), sep, str.substring(0, idx) :: acc)
    }
  }

  def apply(separator: String = DEFAULT_SEP): Bijection[Iterable[String], Option[String]] =
    new AbstractBijection[Iterable[String], Option[String]] {
      override def apply(xs: Iterable[String]) = {
        // TODO: Instead of throwing, escape the separator in the encoded string.
        assert(
          !xs.exists(_.contains(separator)),
          "Can't encode strings that include the separator."
        )
        if (xs.isEmpty)
          None
        else
          Some(xs.mkString(separator))
      }
      override def invert(strOpt: Option[String]) =
        strOpt match {
          case None => Iterable.empty[String]
          // String#split is not reversible, and uses regexs
          case Some(str) => StringJoinBijection.split(str, separator)
        }
    }

}
