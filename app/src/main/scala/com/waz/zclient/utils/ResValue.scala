/**
 * Wire
 * Copyright (C) 2018 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.utils

import com.waz.zclient.R
import android.content.Context

trait ResValue[A] {
  def resId: Int
  def resolve(implicit ctx: Context): A
}

case class ResString(override val resId: Int, quantity: Int, args: ResString.Args) extends ResValue[String] {
  import ResString._

  // in a rare case arguments to a ResString might be ResStrings themselves
  // it shouldn't go deeper than one level, so we shouldn't have to worry about stack overflow
  def resolve(implicit ctx: Context): String = (args, quantity) match {
    case (StringArgs(a), 0) if resId == 0 && a.nonEmpty => a.head
    case (StringArgs(a), 0)                             => ContextUtils.getString(resId, a: _*)
    case (ResStringArgs(a), 0)                          => ContextUtils.getString(resId, a.map(_.resolve): _*)
    case (ResStringArgs(a), q) if q > 0                 => ContextUtils.getQuantityString(resId, q, a.map(_.resolve): _*)
    case (AnyRefArgs(a), q)    if q > 0                 => ContextUtils.getQuantityString(resId, q, a: _*)
    case _ => ""
  }
}

object ResString {
  sealed trait Args

  case class StringArgs   (args: List[String] = Nil)    extends Args
  case class ResStringArgs(args: List[ResString] = Nil) extends Args
  case class AnyRefArgs   (args: List[AnyRef] = Nil)    extends Args

  val Empty: ResString = ResString(R.string.empty_string, 0, StringArgs())

  def apply(resId: Int)                              : ResString = ResString(resId, 0, StringArgs())
  def apply(str: String)                             : ResString = ResString(0,     0, StringArgs(List(str)))
  def apply(resId: Int, args: String*)               : ResString = ResString(resId, 0, StringArgs(args.toList))
  def apply(resId: Int, quantity: Int, args: AnyRef*): ResString = ResString(resId, 0, AnyRefArgs(args.toList))

  // during the compilation both String* and ResString* undergo type erasure to Seq, so both apply(...) would have the same signature if ResString* was used
  def apply(resId: Int, args: List[ResString]): ResString = ResString(resId, 0, ResStringArgs(args))
}
