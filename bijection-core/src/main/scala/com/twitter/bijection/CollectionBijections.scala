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

trait CollectionBijections extends CollectionBijectionsVersionSpecific {

  /*
   * For transformations that may not require a copy, we don't biject on types
   * which would require a copy. To change inner type also, use connect[Seq[T], List[T], List[U]]
   */

  implicit def seq2List[A]: Bijection[Seq[A], List[A]] =
    new AbstractBijection[Seq[A], List[A]] {
      def apply(s: Seq[A]) = s.toList
      override def invert(l: List[A]) = l
    }
  implicit def seq2IndexedSeq[A]: Bijection[Seq[A], IndexedSeq[A]] =
    new AbstractBijection[Seq[A], IndexedSeq[A]] {
      def apply(s: Seq[A]) = s.toIndexedSeq
      override def invert(l: IndexedSeq[A]) = l
    }
  // This doesn't require a copy from Map -> Seq
  implicit def seq2Map[K, V]: Bijection[Seq[(K, V)], Map[K, V]] =
    new AbstractBijection[Seq[(K, V)], Map[K, V]] {
      def apply(s: Seq[(K, V)]) = s.toMap
      override def invert(l: Map[K, V]) = l.toSeq
    }
  // This doesn't require a copy from Set -> Seq
  implicit def seq2Set[T]: Bijection[Seq[T], Set[T]] =
    new AbstractBijection[Seq[T], Set[T]] {
      def apply(s: Seq[T]) = s.toSet
      override def invert(l: Set[T]) = l.toSeq
    }

  implicit def seq2Vector[T]: Bijection[Seq[T], Vector[T]] = trav2Vector[T, Seq[T]]
  implicit def indexedSeq2Vector[T]: Bijection[IndexedSeq[T], Vector[T]] =
    trav2Vector[T, IndexedSeq[T]]

  implicit def betweenMaps[K1, V1, K2, V2](
      implicit kBijection: ImplicitBijection[K1, K2],
      vBijection: ImplicitBijection[V1, V2]
  ) =
    toContainer[(K1, V1), (K2, V2), Map[K1, V1], Map[K2, V2]]

  implicit def betweenVectors[T, U](implicit bij: ImplicitBijection[T, U]) =
    toContainer[T, U, Vector[T], Vector[U]]

  implicit def betweenIndexedSeqs[T, U](implicit bij: ImplicitBijection[T, U]) =
    toContainer[T, U, IndexedSeq[T], IndexedSeq[U]]

  implicit def betweenSets[T, U](implicit bij: ImplicitBijection[T, U]) =
    toContainer[T, U, Set[T], Set[U]]

  implicit def betweenSeqs[T, U](implicit bij: ImplicitBijection[T, U]) =
    toContainer[T, U, Seq[T], Seq[U]]

  implicit def betweenLists[T, U](implicit bij: ImplicitBijection[T, U]) =
    toContainer[T, U, List[T], List[U]]

  implicit def option[T, U](
      implicit bij: ImplicitBijection[T, U]
  ): Bijection[Option[T], Option[U]] =
    new AbstractBijection[Option[T], Option[U]] {
      override def apply(optt: Option[T]) = optt.map(bij.bijection)
      override def invert(optu: Option[U]) = optu.map(bij.bijection.inverse)
    }
  // Always requires a copy
  implicit def vector2List[A, B](
      implicit bij: ImplicitBijection[A, B]
  ): Bijection[Vector[A], List[B]] =
    toContainer[A, B, Vector[A], List[B]]

  implicit def indexedSeq2List[A, B](
      implicit bij: ImplicitBijection[A, B]
  ): Bijection[IndexedSeq[A], List[B]] =
    toContainer[A, B, IndexedSeq[A], List[B]]

}
