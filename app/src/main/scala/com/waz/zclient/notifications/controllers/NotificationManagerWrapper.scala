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
package com.waz.zclient.notifications.controllers

import android.app.{Notification, NotificationManager}
import android.content.Context
import android.graphics.{Color, Typeface}
import android.net.Uri
import android.os.Build
import android.support.v4.app.NotificationCompat.Style
import android.support.v4.app.{NotificationCompat, RemoteInput}
import android.text.style.{ForegroundColorSpan, StyleSpan}
import android.text.{SpannableString, Spanned}
import com.waz.ZLog.ImplicitTag.implicitLogTag
import com.waz.ZLog.warn
import com.waz.model.{ConvId, UserId}
import com.waz.utils.returning
import com.waz.utils.wrappers.Bitmap
import com.waz.zclient.Intents.{CallIntent, QuickReplyIntent}
import com.waz.zclient.utils.ContextUtils.getString
import com.waz.zclient.utils.ResString
import com.waz.zclient.{Intents, R}
import com.waz.zms.NotificationsAndroidService

case class Span(style: Int, range: Int, offset: Int = 0)

object Span {
  val ForegroundColorSpanBlack = 1
  val ForegroundColorSpanGray  = 2
  val StyleSpanBold            = 3
  val StyleSpanItalic          = 4

  val HeaderRange = 1
  val BodyRange   = 2
}

case class SpannableWrapper(header: ResString,
                            body: ResString,
                            spans: List[Span],
                            separator: String) {

  override def toString: String =
    s"""
       |SpannableWrapper(
       |  header:    $header,
       |  body:      $body,
       |  spans:     $spans,
       |  separator: $separator
       |)
     """.stripMargin

  def build(implicit cxt: Context): SpannableString = {
    val headerStr = header.resolve
    val bodyStr = body.resolve
    val wholeStr = headerStr + separator + bodyStr

    def range(span: Span) = span.range match {
      case Span.HeaderRange => (0, headerStr.length)
      case Span.BodyRange   => (headerStr.length + span.offset, wholeStr.length)
    }

    def style(span: Span) = span.style match {
      case Span.ForegroundColorSpanBlack => new ForegroundColorSpan(Color.BLACK)
      case Span.ForegroundColorSpanGray  => new ForegroundColorSpan(Color.GRAY)
      case Span.StyleSpanBold            => new StyleSpan(Typeface.BOLD)
      case Span.StyleSpanItalic          => new StyleSpan(Typeface.ITALIC)
    }

    returning(new SpannableString(wholeStr)) { ss =>
      spans.map(span => (style(span), range(span))).foreach {
        case (style, (start, end)) => ss.setSpan(style, start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE)
      }
    }
  }

  def +(span: Span): SpannableWrapper = copy(spans = this.spans ++ List(span))

  def +(sw: SpannableWrapper): SpannableWrapper = {
    val spans     = this.spans ++ sw.spans.map(span => if (span.range == Span.HeaderRange) span.copy(range = Span.BodyRange) else span)
    val body      = if (sw.header != ResString.Empty) sw.header else sw.body
    copy(spans = spans, body = body)
  }
}

object SpannableWrapper {
  def apply(header: ResString): SpannableWrapper =
    SpannableWrapper(header = header, body = ResString.Empty, spans = List.empty, separator = "")
  def apply(header: ResString, spans: List[Span]): SpannableWrapper =
    SpannableWrapper(header = header, body = ResString.Empty, spans = List.empty, separator = "")

  val Empty: SpannableWrapper = SpannableWrapper(ResString.Empty)
}

case class StyleBuilder(style: Int,
                        title: SpannableWrapper,
                        summaryText: Option[String] = None,
                        bigText: Option[SpannableWrapper] = None,
                        lines: List[SpannableWrapper] = List.empty) {
  override def toString: String = {
    val sb = StringBuilder.newBuilder
    sb.append("StyleBuilder(\n")
    sb.append("  style: ").append(style).append(",\n")
    sb.append("  title: ").append(title).append(",\n")
    summaryText.foreach(v => sb.append("  summaryText: ").append(v).append(",\n"))
    bigText    .foreach(v => sb.append("  bigText: ").append(v).append(",\n"))
    sb.append("  lines: ").append(lines.toString).append(",\n")
    sb.append(")")
    sb.toString()
  }

  def build(implicit cxt: Context): Style = style match {
    case StyleBuilder.BigText =>
      returning(new NotificationCompat.BigTextStyle) { bts =>
        bts.setBigContentTitle(title.build)
        summaryText.foreach(bts.setSummaryText)
        bigText.map(_.build).foreach(bts.bigText(_))
      }
    case StyleBuilder.Inbox =>
      returning(new NotificationCompat.InboxStyle) { is =>
        is.setBigContentTitle(title.build)
        summaryText.foreach(is.setSummaryText)
        lines.map(_.build).foreach(is.addLine(_))
      }
    }
}

object StyleBuilder {
  val BigText = 1
  val Inbox   = 2
}

case class NotificationProps(when:                     Option[Long] = None,
                             showWhen:                 Option[Boolean] = None,
                             category:                 Option[String] = None,
                             priority:                 Option[Int] = None,
                             smallIcon:                Option[Int] = None,
                             contentTitle:             Option[SpannableWrapper] = None,
                             contentText:              Option[SpannableWrapper] = None,
                             style:                    Option[StyleBuilder] = None,
                             groupSummary:             Option[Boolean] = None,
                             group:                    Option[UserId] = None,
                             openAccountIntent:        Option[UserId] = None,
                             clearNotificationsIntent: Option[(UserId, Option[ConvId])] = None,
                             openConvIntent:           Option[(UserId, ConvId, Int)] = None,
                             contentInfo:              Option[String] = None,
                             color:                    Option[Int] = None,
                             vibrate:                  Option[Array[Long]] = None,
                             autoCancel:               Option[Boolean] = None,
                             sound:                    Option[Uri] = None,
                             onlyAlertOnce:            Option[Boolean] = None,
                             lights:                   Option[(Int, Int, Int)] = None,
                             largeIcon:                Option[Bitmap] = None,
                             action1:                  Option[(UserId, ConvId, Int, Boolean)] = None,
                             action2:                  Option[(UserId, ConvId, Int, Boolean)] = None
                            ) {
  override def toString: String = {
    val sb = StringBuilder.newBuilder
    sb.append("NotificationProps(\n")
    when                    .foreach(v => sb.append("  when: ").append(v).append(",\n"))
    showWhen                .foreach(v => sb.append("  showWhen: ").append(v).append(",\n"))
    category                .foreach(v => sb.append("  category: ").append(v).append(",\n"))
    priority                .foreach(v => sb.append("  priority: ").append(v).append(",\n"))
    smallIcon               .foreach(v => sb.append("  smallIcon: ").append(v).append(",\n"))
    contentTitle            .foreach(v => sb.append("  contentTitle: ").append(v).append(",\n"))
    contentText             .foreach(v => sb.append("  contentText: ").append(v).append(",\n"))
    style                   .foreach(v => sb.append("  style: ").append(v).append(",\n"))
    groupSummary            .foreach(v => sb.append("  groupSummary: ").append(v).append(",\n"))
    group                   .foreach(v => sb.append("  group: ").append(v).append(",\n"))
    openAccountIntent       .foreach(v => sb.append("  openAccountIntent: ").append(v).append(",\n"))
    clearNotificationsIntent.foreach(v => sb.append("  clearNotificationsIntent: ").append(v).append(",\n"))
    openConvIntent          .foreach(v => sb.append("  openConvIntent: ").append(v).append(",\n"))
    contentInfo             .foreach(v => sb.append("  contentInfo: ").append(v).append(",\n"))
    color                   .foreach(v => sb.append("  color: ").append(v).append(",\n"))
    vibrate                 .foreach(v => sb.append("  vibrate: ").append(v).append(",\n"))
    autoCancel              .foreach(v => sb.append("  autoCancel: ").append(v).append(",\n"))
    sound                   .foreach(v => sb.append("  sound: ").append(v).append(",\n"))
    onlyAlertOnce           .foreach(v => sb.append("  onlyAlertOnce: ").append(v).append(",\n"))
    lights                  .foreach(v => sb.append("  lights: ").append(v).append(",\n"))
    largeIcon               .foreach(v => sb.append("  largeIcon: ").append(v).append(",\n"))
    action1                 .foreach(v => sb.append("  action1: ").append(v).append(",\n"))
    action2                 .foreach(v => sb.append("  action2: ").append(v).append(",\n"))
    sb.append(")")
    sb.toString()
  }

  def build(implicit cxt: Context): Notification = {
    val builder = new NotificationCompat.Builder(cxt, null)

    when.foreach(builder.setWhen)
    showWhen.foreach(builder.setShowWhen)
    category.foreach(builder.setCategory)
    priority.foreach(builder.setPriority)
    smallIcon.foreach(builder.setSmallIcon)
    contentTitle.map(_.build).foreach(builder.setContentTitle)
    contentText.map(_.build).foreach(builder.setContentText)
    style.map(_.build).foreach(builder.setStyle)
    groupSummary.foreach(builder.setGroupSummary)
    group.foreach(accountId => builder.setGroup(accountId.str))

    openAccountIntent.foreach(userId => builder.setContentIntent(Intents.OpenAccountIntent(userId)))

    clearNotificationsIntent.foreach {
      case (accountId, Some(convId)) => builder.setDeleteIntent(NotificationsAndroidService.clearNotificationsIntent(accountId, convId, cxt))
      case (accountId, None)         => builder.setDeleteIntent(NotificationsAndroidService.clearNotificationsIntent(accountId, cxt))
    }

    openConvIntent.foreach {
      case (accountId, convId, requestBase) => builder.setContentIntent(Intents.OpenConvIntent(accountId, convId, requestBase))
    }

    contentInfo.foreach(builder.setContentInfo)
    color.foreach(builder.setColor)
    vibrate.foreach(builder.setVibrate)
    autoCancel.foreach(builder.setAutoCancel)
    sound.foreach(builder.setSound)
    onlyAlertOnce.foreach(builder.setOnlyAlertOnce)
    lights.foreach { case (c, on, off) => builder.setLights(c, on, off) }
    largeIcon.foreach(bmp => builder.setLargeIcon(bmp))

    action1.map {
      case (userId, convId, requestBase, _) =>
        new NotificationCompat.Action.Builder(R.drawable.ic_action_call, getString(R.string.notification__action__call), CallIntent(userId, convId, requestBase)).build()
    }.foreach(builder.addAction)

    action2.map {
      case (userId, convId, requestBase, bundleEnabled) => createQuickReplyAction(userId, convId, requestBase, bundleEnabled)
    }.foreach(builder.addAction)

    builder.build()
  }

  private def createQuickReplyAction(userId: UserId, convId: ConvId, requestCode: Int, bundleEnabled: Boolean)(implicit cxt: Context) = {
    if (bundleEnabled) {
      val remoteInput = new RemoteInput.Builder(NotificationsAndroidService.InstantReplyKey)
        .setLabel(getString(R.string.notification__action__reply))
        .build
      new NotificationCompat.Action.Builder(R.drawable.ic_action_reply, getString(R.string.notification__action__reply), NotificationsAndroidService.quickReplyIntent(userId, convId, cxt))
        .addRemoteInput(remoteInput)
        .setAllowGeneratedReplies(true)
        .build()
    } else
      new NotificationCompat.Action.Builder(R.drawable.ic_action_reply, getString(R.string.notification__action__reply), QuickReplyIntent(userId, convId, requestCode)).build()
  }
}

trait NotificationManagerWrapper {
  def cancel(id: Int): Unit
  def getActiveNotificationIds: Seq[Int]
  def notify(id: Int, props: NotificationProps)(implicit ctx: Context): Unit
}

object NotificationManagerWrapper {
  class AndroidNotificationsManager(notificationManager: NotificationManager) extends NotificationManagerWrapper {
    override def cancel(id: Int): Unit = notificationManager.cancel(id)

    override def getActiveNotificationIds: Seq[Int] =
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)
        notificationManager.getActiveNotifications.toSeq.map(_.getId)
      else {
        warn(s"Tried to access method getActiveNotifications from api level: ${Build.VERSION.SDK_INT}")
        Seq.empty
      }

    override def notify(id: Int, props: NotificationProps)(implicit ctx: Context): Unit =
      notificationManager.notify(id, props.build)
  }
}

