// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
package com.digitalasset.daml.lf.data

import scalaz.Equal

sealed abstract class StringModule {

  type T <: String

  def fromString(s: String): Either[String, T]

  @throws[IllegalArgumentException]
  final def assertFromString(s: String): T =
    assertRight(fromString(s))

  def equalInstance: Equal[T]

  final implicit def ordering: Ordering[T] = _ compare _

  // We provide the following array factory instead of a ClassTag
  // because the latter lets people easily reinterpret any string as a T.
  // See
  //  * https://github.com/digital-asset/daml/pull/983#discussion_r282513324
  //  * https://github.com/scala/bug/issues/9565
  val Array: ArrayFactory[T]

  final def toStringMap[V](map: Map[T, V]): Map[String, V] = map.toMap
}

private sealed trait StringModuleImpl {
  type T = String

  def equalInstance: Equal[T] = scalaz.std.string.stringInstance

  val Array: ArrayFactory[T] = new ArrayFactory[T]
}

object MatchingStringModule extends (String => StringModule) {

  override def apply(string_regex: String): StringModule =
    new StringModule with StringModuleImpl {
      private val regex = string_regex.r
      private val pattern = regex.pattern

      override def fromString(s: String): Either[String, T] =
        Either.cond(
          pattern.matcher(s).matches(),
          s,
          s"""string "$s" does not match regex "$regex"""",
        )
    }

}

sealed abstract class ConcatenableStringModule extends StringModule {

  def fromLong(i: Long): T

  final def fromInt(i: Int): T = fromLong(i.toLong)

  def concat(s: T, ss: T*): T
}

/** ConcatenableMatchingString are non empty US-ASCII strings built with letters, digits,
  * and some other (parameterizable) extra characters.
  * We use them to represent identifiers. In this way, we avoid
  * empty identifiers, escaping problems, and other similar pitfalls.
  *
  * ConcatenableMatchingString has the advantage over MatchingStringModule of being
  * concatenable and can be generated from numbers without extra checks.
  * Those properties are heavily use to generate some ids by combining other existing
  * ids.
  */
object ConcatenableMatchingStringModule
    extends ((Char => Boolean, Int) => ConcatenableStringModule) {

  override def apply(
      extraAllowedChars: Char => Boolean,
      maxLength: Int = Int.MaxValue,
  ): ConcatenableStringModule =
    new ConcatenableStringModule with StringModuleImpl {

      override def fromString(s: String): Either[String, T] =
        if (s.isEmpty)
          Left(s"""empty string""")
        else if (s.length > maxLength)
          Left(s"""string too long""")
        else
          s.find(c => c > 0x7f || !(c.isLetterOrDigit || extraAllowedChars(c)))
            .fold[Either[String, T]](Right(s))(c =>
              Left(s"""non expected character 0x${c.toInt.toHexString} in "$s""""),
            )

      override def fromLong(i: Long): T = i.toString

      override def concat(s: T, ss: T*): T = {
        val b = StringBuilder.newBuilder
        b ++= s
        ss.foreach(b ++= _)
        b.result()
      }
    }

}

sealed abstract class ContractIdStringModule[CidV0 <: String, CidV1 <: String]
    extends StringModule {
  def fromV0(s: CidV0): T
  def fromV1(s: CidV1): T

  val fromStringToV0: String => Either[String, T]
  val fromStringToV1: String => Either[String, T]
  val prefixV1: String

  override final def fromString(s: String): Either[String, T] =
    if (s.startsWith(prefixV1))
      fromStringToV1(s)
    else
      fromStringToV0(s)

}

object ContractIdStringModule {

  def apply[CidV0 <: String, CidV1 <: String](
      fromStringToV0_ : String => Either[String, CidV0],
      fromStringToV1_ : String => Either[String, CidV1],
      prefixV1_ : String,
  ): ContractIdStringModule[CidV0, CidV1] =
    new ContractIdStringModule[CidV0, CidV1] with StringModuleImpl {
      override val fromStringToV0: String => Either[String, T] = fromStringToV0_
      override val fromStringToV1: String => Either[String, T] = fromStringToV1_
      override val prefixV1: String = prefixV1_
      override def fromV0(s: CidV0): T = s
      override def fromV1(s: CidV1): T = s
    }

}
