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

import java.lang.{Iterable => JIterable}
import java.util.{
  Collection => JCollection,
  Dictionary => JDictionary,
  Enumeration => JEnumeration,
  Iterator => JIterator,
  List => JList,
  Map => JMap,
  Set => JSet
}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.collection.Factory
import scala.reflect.ClassTag

trait CollectionBijectionsVersionSpecific extends BinaryBijections {

  /*
   * Bijections between collection types defined in scala.collection.JavaConverters.
   * These are version-specific because they require scala.collection.JavaConverters._
   * vs scala.jdk.CollectionConverters._
   */

  implicit def iterable2java[T]: Bijection[Iterable[T], JIterable[T]] =
    new AbstractBijection[Iterable[T], JIterable[T]] {
      override def apply(t: Iterable[T]) = t.asJava
      override def invert(u: JIterable[T]) = u.asScala
    }

  implicit def iterator2java[T]: Bijection[Iterator[T], JIterator[T]] =
    new AbstractBijection[Iterator[T], JIterator[T]] {
      override def apply(t: Iterator[T]) = t.asJava
      override def invert(u: JIterator[T]) = u.asScala
    }
  implicit def buffer2java[T]: Bijection[mutable.Buffer[T], JList[T]] =
    new AbstractBijection[mutable.Buffer[T], JList[T]] {
      override def apply(t: mutable.Buffer[T]) = t.asJava
      override def invert(u: JList[T]) = u.asScala
    }
  implicit def mset2java[T]: Bijection[mutable.Set[T], JSet[T]] =
    new AbstractBijection[mutable.Set[T], JSet[T]] {
      override def apply(t: mutable.Set[T]) = t.asJava
      override def invert(u: JSet[T]) = u.asScala
    }
  implicit def mmap2java[K, V]: Bijection[mutable.Map[K, V], JMap[K, V]] =
    new AbstractBijection[mutable.Map[K, V], JMap[K, V]] {
      override def apply(t: mutable.Map[K, V]) = t.asJava
      override def invert(t: JMap[K, V]) = t.asScala
    }
  implicit def iterable2jcollection[T]: Bijection[Iterable[T], JCollection[T]] =
    new AbstractBijection[Iterable[T], JCollection[T]] {
      override def apply(t: Iterable[T]) = t.asJavaCollection
      override def invert(u: JCollection[T]) = u.asScala
    }
  implicit def iterator2jenumeration[T]: Bijection[Iterator[T], JEnumeration[T]] =
    new AbstractBijection[Iterator[T], JEnumeration[T]] {
      override def apply(t: Iterator[T]) = t.asJavaEnumeration
      override def invert(u: JEnumeration[T]) = u.asScala
    }
  implicit def mmap2jdictionary[K, V]: Bijection[mutable.Map[K, V], JDictionary[K, V]] =
    new AbstractBijection[mutable.Map[K, V], JDictionary[K, V]] {
      override def apply(t: mutable.Map[K, V]) = t.asJavaDictionary
      override def invert(t: JDictionary[K, V]) = t.asScala
    }
  // Immutable objects (they copy from java to scala):
  implicit def seq2Java[T]: Bijection[Seq[T], JList[T]] =
    new AbstractBijection[Seq[T], JList[T]] {
      def apply(s: Seq[T]) = s.asJava
      override def invert(l: JList[T]) = l.asScala.toSeq
    }
  implicit def set2Java[T]: Bijection[Set[T], JSet[T]] =
    new AbstractBijection[Set[T], JSet[T]] {
      def apply(s: Set[T]) = s.asJava
      override def invert(l: JSet[T]) = l.asScala.toSet
    }
  implicit def map2Java[K, V]: Bijection[Map[K, V], JMap[K, V]] =
    new AbstractBijection[Map[K, V], JMap[K, V]] {
      def apply(s: Map[K, V]) = s.asJava
      override def invert(l: JMap[K, V]) = l.asScala.toMap
    }

  //Traversable is version-specific
  protected def trav2Vector[T, C >: Vector[T] <: Iterable[T]]: Bijection[C, Vector[T]] =
    new SubclassBijection[C, Vector[T]](classOf[Vector[T]]) {
      def applyfn(s: C) = {
        // Just build one:
        val bldr = new scala.collection.immutable.VectorBuilder[T]
        bldr ++= s
        bldr.result
      }
    }

  /**
    * Accepts a Bijection[A, B] and returns a bijection that can
    * transform Iterable containers of A into Iterable containers of B.
    *
    * Do not go from ordered to unordered containers;
    * Bijection[Iterable[A], Set[B]] is inaccurate, and really makes
    * no sense.
    */
  def toContainer[A, B, C <: IterableOnce[A], D <: IterableOnce[B]](
      implicit bij: ImplicitBijection[A, B],
      cd: Factory[B, D],
      dc: Factory[A, C]
  ): Bijection[C, D] =
    new AbstractBijection[C, D] {
      def apply(c: C) = {
        val builder = cd()
        c foreach { builder += bij(_) }
        builder.result()
      }
      override def invert(d: D) = {
        val builder = dc()
        d foreach { builder += bij.invert(_) }
        builder.result()
      }
    }

  /**
    * This doesn't actually copy the Array, only wraps/unwraps with WrappedArray
    */
  implicit def array2Traversable[T: ClassTag]: Bijection[Array[T], Iterable[T]] =
    new AbstractBijection[Array[T], Iterable[T]] {
      override def apply(a: Array[T]) = ArraySeq.unsafeWrap(a)
      override def invert(t: Iterable[T]) = t match {
        case as: ArraySeq[T] => as.unwrap
        case _               => t.toArray
      }
    }

  /**
    * This doesn't actually copy the Array, only wraps/unwraps with WrappedArray
    */
  implicit def array2Seq[T: ClassTag]: Bijection[Array[T], Seq[T]] =
    new AbstractBijection[Array[T], Seq[T]] {
      override def apply(a: Array[T]) = ArraySeq.unsafeWrap(a)
      override def invert(t: Iterable[T]) = t match {
        case as: ArraySeq[T] => as.unwrap
        case _               => t.toArray
      }
    }
}
