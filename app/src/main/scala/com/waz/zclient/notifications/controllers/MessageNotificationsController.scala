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

import android.annotation.TargetApi
import android.content.Context
import android.graphics._
import android.net.Uri
import android.os.Build
import android.support.annotation.RawRes
import android.support.v4.app.NotificationCompat
import android.text.TextUtils
import com.waz.ZLog.ImplicitTag.implicitLogTag
import com.waz.ZLog.verbose
import com.waz.api.NotificationsHandler.NotificationType._
import com.waz.bitmap.BitmapUtils
import com.waz.content.{AssetsStorage, ConversationStorage, TeamsStorage, UsersStorage}
import com.waz.model._
import com.waz.service.images.ImageLoader
import com.waz.service.push.GlobalNotificationsService
import com.waz.service.push.NotificationService.NotificationInfo
import com.waz.service.{AccountsService, UiLifeCycle}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache.BitmapRequest
import com.waz.utils._
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.Bitmap
import com.waz.zclient.WireApplication.{AccountToAssetsStorage, AccountToImageLoader}
import com.waz.zclient._
import com.waz.zclient.common.controllers.SoundController
import com.waz.zclient.common.controllers.global.AccentColorController
import com.waz.zclient.controllers.navigation.Page
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.messages.controllers.NavigationController
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.utils.{ResString, RingtoneUtils}

import scala.concurrent.Future
import scala.concurrent.duration._

class MessageNotificationsController(bundleEnabled: Boolean = Build.VERSION.SDK_INT > Build.VERSION_CODES.M,
                                     applicationId: String = BuildConfig.APPLICATION_ID)
                                    (implicit inj: Injector, cxt: Context, eventContext: EventContext) extends Injectable { self =>

  import MessageNotificationsController._
  import Threading.Implicits.Background

  private lazy val notificationsService  = inject[GlobalNotificationsService]
  private lazy val accounts              = inject[AccountsService]
  private lazy val selfId                = inject[Signal[UserId]]
  private lazy val soundController       = inject[SoundController]
  private lazy val navigationController  = inject[NavigationController]
  private lazy val convController        = inject[ConversationController]
  private lazy val convsStorage          = inject[Signal[ConversationStorage]]
  private lazy val userStorage           = inject[Signal[UsersStorage]]
  private lazy val teamsStorage          = inject[TeamsStorage]

  private val notManager                 = inject[NotificationManagerWrapper]

  private var accentColors = Map[UserId, Int]()

  inject[AccentColorController].colors { cs =>
    accentColors = cs.map { case (userId, color) => userId -> color.getColor }
  }

  notificationsService.groupedNotifications.onUi { notifications =>
    notifications.toSeq.sortBy(_._1.str.hashCode).foreach {
      case (userId, (shouldBeSilent, nots)) =>
        verbose(s"bundleEnabled: $bundleEnabled")
        verbose(s"notifications received: ${nots.map(n => (n.id, n.message, n.convId, n.userName))}")
        for {
          userStorage  <- userStorage.head
          user         <- userStorage.optSignal(userId).head
          team         <- user.flatMap(_.teamId) match {
            case Some(teamId) => teamsStorage.get(teamId)
            case _            => Future.successful(Option.empty[TeamData])
          }
          _ = createConvNotifications(userId, shouldBeSilent, nots, team.map(_.name))
        } yield ()
    }
  }

  private val conversationsBeingDisplayed: Signal[Map[UserId, Set[ConvId]]] = for {
    selfId   <- selfId
    accs     <- accounts.accountsWithManagers
    uiActive <- inject[UiLifeCycle].uiActive
    convId   <- convController.currentConvId.map(Option(_)).orElse(Signal.const(Option.empty[ConvId]))
    convs    <- convsStorage.map(_.conversations)
    page     <- navigationController.visiblePage
  } yield
    accs.map { accId =>
      accId ->
        (if (selfId != accId || !uiActive) Set.empty[ConvId]
        else page match {
          case Page.CONVERSATION_LIST => convs.map(_.id).toSet
          case Page.MESSAGE_STREAM    => Set(convId).flatten
          case _                      => Set.empty[ConvId]
        })
    }.toMap

  conversationsBeingDisplayed { displayed =>
    notificationsService.notificationsSourceVisible ! displayed

    displayed.foreach {
      case (userId, convIds) =>
        convIds.foreach { convId => notManager.cancel(toNotificationConvId(userId, convId)) }
    }

  }

  private def getPictureForNotifications(userId: UserId, nots: Seq[NotificationInfo]): Future[Option[Bitmap]] =
    if (nots.exists(_.isEphemeral)) Future.successful(None)
    else {
      val pictures  = nots.flatMap(_.userPicture).distinct
      val assetId   = if (pictures.size == 1) pictures.headOption else None

      verbose(s"getPictureForNotifications, assetId: $assetId")

      val imageLoader   = inject[AccountToImageLoader]
      val assetsStorage = inject[AccountToAssetsStorage]

      Future.sequence(List(imageLoader(userId), assetsStorage(userId))).flatMap {
        case Some(imageLoader: ImageLoader) :: Some(assetsStorage: AssetsStorage) :: Nil =>
          for {
            assetData <- assetId.fold(Future.successful(Option.empty[AssetData]))(assetsStorage.get)
            bmp       <- assetData.fold(Future.successful(Option.empty[Bitmap])){ ad =>
              imageLoader.loadBitmap(ad, BitmapRequest.Single(toPx(64)), forceDownload = false).map(Option(_)).withTimeout(500.millis).recoverWith {
                case _ : Throwable => CancellableFuture.successful(None)
              }.future
            }
          } yield
            bmp.map { original => Bitmap.fromAndroid(BitmapUtils.createRoundBitmap(original, toPx(64), 0, Color.TRANSPARENT)) }
        case _ => Future.successful(None)
      }
    }

  private def createSummaryNotificationProps(userId: UserId, silent: Boolean, nots: Seq[NotificationInfo], teamName: Option[String]) =
    NotificationProps (
      when                     = Some(nots.minBy(_.time).time.toEpochMilli),
      showWhen                 = Some(true),
      category                 = Some(NotificationCompat.CATEGORY_MESSAGE),
      priority                 = Some(NotificationCompat.PRIORITY_HIGH),
      smallIcon                = Some(R.drawable.ic_menu_logo),
      groupSummary             = Some(true),
      group                    = Some(userId),
      openAccountIntent        = Some(userId),
      clearNotificationsIntent = Some((userId, None)),
      contentInfo              = teamName,
      color                    = notificationColor(userId)
    )

  private def createConvNotifications(userId: UserId, silent: Boolean, nots: Seq[NotificationInfo], teamName: Option[String]): Unit =
    if (nots.isEmpty) Future.successful(notManager.cancel(toNotificationGroupId(userId)))
    else {
      verbose(s"bundleEnabled: $bundleEnabled")
      val groupedConvs =
        if (bundleEnabled)
          nots.groupBy(_.convId).map {
            case (convId, ns) => toNotificationConvId(userId, convId) -> ns
          }
        else Map(toNotificationGroupId(userId) -> nots)

      verbose(s"groupedConvs = $groupedConvs")

      val teamNameOpt = if (groupedConvs.keys.size > 1) None else teamName

      if (bundleEnabled) {
        val notIdSet = groupedConvs.keys.toSet
        notManager.getActiveNotificationIds
          .collect { case id if !notIdSet.contains(id) => id }
          .foreach(notManager.cancel)

        val summaryId = toNotificationGroupId(userId)
        if (notIdSet.contains(summaryId)) notManager.cancel(summaryId)

        val summary = createSummaryNotificationProps(userId, silent, nots, teamName)
        verbose(s"summary: $summary")
        notManager.notify(summaryId, summary)
      }

      val notFutures = groupedConvs.map { case (notId, ns) =>
        getPictureForNotifications(userId, ns).map { pic =>
          val commonProps = commonNotificationProperties(ns, userId, silent, pic)
          val specificProps = if (ns.size == 1)
            singleNotificationProperties(commonProps, userId, ns.head, teamNameOpt)
          else
            multipleNotificationProperties(commonProps, userId, ns, teamNameOpt)
          verbose(s"creating props in the future: $specificProps for ${ns.map(_.id)}")
          notId -> (specificProps, ns.map(_.id))
        }
      }

      Future.sequence(notFutures).foreach {
        _.foreach { case (notId, (props, notsToCancel)) =>
          verbose(s"notifying about props: $notId -> $props")
          notManager.notify(notId, props)
          notificationsService.markAsDisplayed(userId, notsToCancel)
        }
      }(Threading.Ui)
    }

  private def singleNotificationProperties(props: NotificationProps, userId: UserId, n: NotificationInfo, teamName: Option[String]): NotificationProps = {
    verbose("getSingleMessageNotification")
    val title        = SpannableWrapper(getMessageTitle(n, None), List(Span(Span.StyleSpanBold, Span.HeaderRange)))
    val body         = getMessage(n, singleConversationInBatch = true)
    val requestBase  = System.currentTimeMillis.toInt
    val bigTextStyle = StyleBuilder(StyleBuilder.BigText, title = title, summaryText = teamName, bigText = Some(body))

    val specProps = props.copy(
      contentTitle             = Some(title),
      contentText              = Some(body),
      style                    = Some(bigTextStyle),
      openConvIntent           = Some((userId, n.convId, requestBase)),
      clearNotificationsIntent = Some((userId, Some(n.convId)))
    )

    if (n.tpe != CONNECT_REQUEST)
      specProps.copy(
        action1 = Some((userId, n.convId, requestBase + 1, bundleEnabled)),
        action2 = Some((userId, n.convId, requestBase + 2, bundleEnabled))
      )
    else specProps
  }

  private def multipleNotificationProperties(props: NotificationProps, userId: UserId, ns: Seq[NotificationInfo], teamName: Option[String]): NotificationProps = {
    verbose("getMultipleMessagesNotification")
    val convIds = ns.map(_.convId).toSet
    val isSingleConv = convIds.size == 1

    val header =
      if (isSingleConv) {
        if (ns.exists(_.isEphemeral))
          ResString(R.string.notification__message__ephemeral_someone)
        else if (ns.head.isGroupConv)
          ns.head.convName.map(ResString(_)).getOrElse(ResString.Empty)
        else
          ns.head.convName.orElse(ns.head.userName).map(ResString(_)).getOrElse(ResString.Empty)
      }
      else
        ResString(R.plurals.notification__new_messages__multiple, ns.size, convIds.size.toString)

    val separator = " • "

    val title =
      if (isSingleConv && ns.size > 5)
        SpannableWrapper(
          header = header,
          body = ResString(R.plurals.conversation_list__new_message_count, ns.size, Nil),
          spans = List(
            Span(Span.StyleSpanBold, Span.HeaderRange),
            Span(Span.StyleSpanItalic, Span.BodyRange, separator.length),
            Span(Span.ForegroundColorSpanGray, Span.BodyRange)
          ),
          separator = separator
        )
      else
        SpannableWrapper(
          header = header,
          spans = List(Span(Span.StyleSpanBold, Span.HeaderRange))
        )

    val requestBase = System.currentTimeMillis.toInt
    val messages    = ns.sortBy(_.time).map(n => getMessage(n, singleConversationInBatch = isSingleConv)).takeRight(5).toList
    val inboxStyle  = StyleBuilder(StyleBuilder.Inbox, title = title, summaryText = if (bundleEnabled) teamName else None, lines = messages)

    val specProps = props.copy(
      contentTitle = Some(title),
      contentText  = Some(messages.last),
      style        = Some(inboxStyle)
    )

    if (isSingleConv)
      specProps.copy(
        openConvIntent           = Some((userId, convIds.head, requestBase)),
        clearNotificationsIntent = Some((userId, Some(convIds.head))),
        action1                  = Some((userId, convIds.head, requestBase + 1, bundleEnabled)),
        action2                  = Some((userId, convIds.head, requestBase + 2, bundleEnabled))
      )
    else
      specProps.copy(
        openAccountIntent        = Some(userId),
        clearNotificationsIntent = Some((userId, None))
      )
  }

  private def getSelectedSoundUri(value: String, @RawRes defaultResId: Int): Uri =
    getSelectedSoundUri(value, defaultResId, defaultResId)

  private def getSelectedSoundUri(value: String, @RawRes preferenceDefault: Int, @RawRes returnDefault: Int): Uri = {
    verbose(s"getSelectedSoundUri($value, $preferenceDefault, $returnDefault")
    if (!TextUtils.isEmpty(value) && !RingtoneUtils.isDefaultValue(cxt, value, preferenceDefault)) Uri.parse(value)
    else RingtoneUtils.getUriForRawId(cxt, returnDefault)
  }

  private def commonNotificationProperties(ns: Seq[NotificationInfo], userId: UserId, silent: Boolean, pic: Option[Bitmap]) = {
    verbose(s"commonNotificationProperties")
    val color = notificationColor(userId)
    NotificationProps(
      showWhen      = Some(true),
      category      = Some(NotificationCompat.CATEGORY_MESSAGE),
      priority      = Some(NotificationCompat.PRIORITY_HIGH),
      smallIcon     = Some(R.drawable.ic_menu_logo),
      vibrate       = if (!silent && soundController.isVibrationEnabled) Some(getIntArray(R.array.new_message_gcm).map(_.toLong)) else Some(Array(0l,0l)),
      autoCancel    = Some(true),
      sound         = getSound(ns, silent),
      onlyAlertOnce = Some(ns.forall(_.hasBeenDisplayed)),
      group         = Some(userId),
      when          = Some(ns.maxBy(_.time).time.toEpochMilli),
      largeIcon     = pic,
      lights        = color.map(c => (c, getInt(R.integer.notifications__system__led_on), getInt(R.integer.notifications__system__led_off))),
      color         = color
    )
  }

  private def getSound(ns: Seq[NotificationInfo], silent: Boolean) = {
   if (soundController.soundIntensityNone || silent) None
    else if (!soundController.soundIntensityFull && (ns.size > 1 && ns.lastOption.forall(_.tpe != KNOCK))) None
    else ns.map(_.tpe).lastOption.fold(Option.empty[Uri]) {
      case ASSET | ANY_ASSET | VIDEO_ASSET | AUDIO_ASSET |
           LOCATION | TEXT | CONNECT_ACCEPTED | CONNECT_REQUEST | RENAME |
           LIKE  => Option(getSelectedSoundUri(soundController.currentTonePrefs._2, R.raw.new_message_gcm))
      case KNOCK => Option(getSelectedSoundUri(soundController.currentTonePrefs._3, R.raw.ping_from_them))
      case _     => None
    }
  }

  private[notifications] def getMessage(n: NotificationInfo, singleConversationInBatch: Boolean): SpannableWrapper = {
    verbose(s"getMessage")
    val message = n.message.replaceAll("\\r\\n|\\r|\\n", " ")
    val header  = n.tpe match {
      case CONNECT_ACCEPTED => ResString.Empty
      case _                => getDefaultNotificationMessageLineHeader(n, singleConversationInBatch)
    }

    val body = n.tpe match {
      case _ if n.isEphemeral => ResString(R.string.conversation_list__ephemeral)
      case TEXT               => ResString(message)
      case MISSED_CALL        => ResString(R.string.notification__message__one_to_one__wanted_to_talk)
      case KNOCK              => ResString(R.string.notification__message__one_to_one__pinged)
      case ANY_ASSET          => ResString(R.string.notification__message__one_to_one__shared_file)
      case ASSET              => ResString(R.string.notification__message__one_to_one__shared_picture)
      case VIDEO_ASSET        => ResString(R.string.notification__message__one_to_one__shared_video)
      case AUDIO_ASSET        => ResString(R.string.notification__message__one_to_one__shared_audio)
      case LOCATION           => ResString(R.string.notification__message__one_to_one__shared_location)
      case RENAME             => ResString(R.string.notification__message__group__renamed_conversation, message)
      case MEMBER_LEAVE       => ResString(R.string.notification__message__group__remove)
      case MEMBER_JOIN        => ResString(R.string.notification__message__group__add)
      case LIKE if n.likedContent.nonEmpty =>
        n.likedContent.collect {
          case LikedContent.PICTURE =>
            ResString(R.string.notification__message__liked_picture)
          case LikedContent.TEXT_OR_URL =>
            ResString(R.string.notification__message__liked, n.message)
        }.getOrElse(ResString(R.string.notification__message__liked_message))
      case CONNECT_ACCEPTED       => ResString(R.string.notification__message__single__accept_request, n.userName.getOrElse(""))
      case CONNECT_REQUEST        => ResString(R.string.people_picker__invite__share_text__header, n.userName.getOrElse(""))
      case MESSAGE_SENDING_FAILED => ResString(R.string.notification__message__send_failed)
      case _ => ResString.Empty
    }

    getMessageSpannable(header, body, n.tpe == TEXT)
  }

  private def getMessageTitle(n: NotificationInfo, teamName: Option[String]): ResString = {
    verbose(s"getMessageTitle")
    if (n.isEphemeral)
      ResString(R.string.notification__message__ephemeral_someone)
    else {
      val convName = n.convName.orElse(n.userName).getOrElse("")
      teamName match {
        case Some(name) =>
          ResString(R.string.notification__message__group__prefix__other, convName, name)
        case None =>
          ResString(convName)
      }
    }
  }

  @TargetApi(21)
  private def getMessageSpannable(header: ResString, body: ResString, isTextMessage: Boolean) = {
    val spans = Span(Span.ForegroundColorSpanBlack, Span.HeaderRange) ::
      (if (!isTextMessage) List(Span(Span.StyleSpanItalic, Span.BodyRange)) else Nil)
    SpannableWrapper(header = header, body = body, spans = spans, separator = "")
  }

  private def getDefaultNotificationMessageLineHeader(n: NotificationInfo, singleConversationInBatch: Boolean): ResString =
    if (n.isEphemeral) ResString.Empty
    else {
      val prefixId =
        if (!singleConversationInBatch && n.isGroupConv) R.string.notification__message__group__prefix__text
        else if (!singleConversationInBatch && !n.isGroupConv || singleConversationInBatch && n.isGroupConv) R.string.notification__message__name__prefix__text
        else 0
      if (prefixId > 0) {
        val userName = n.userName.getOrElse("")
        n.convName match {
          case Some(convName) => ResString(prefixId, userName, convName)
          case None           => ResString(prefixId, List(ResString(userName), ResString(R.string.notification__message__group__default_conversation_name)))
        }
      }
      else ResString.Empty
    }

  private def notificationColor(userId: UserId) = applicationId match {
    case "com.wire.internal"   => Some(Color.GREEN)
    case "com.waz.zclient.dev" => accentColors.get(userId)
    case "com.wire.x"          => Some(Color.RED)
    case "com.wire.qa"         => Some(Color.BLUE)
    case _                     => None
  }

}

object MessageNotificationsController {
  def toNotificationGroupId(userId: UserId): Int = userId.str.hashCode()
  def toNotificationConvId(userId: UserId, convId: ConvId): Int = (userId.str + convId.str).hashCode()
  def channelId(userId: UserId): String = userId.str

  val ZETA_MESSAGE_NOTIFICATION_ID: Int = 1339272
  val ZETA_EPHEMERAL_NOTIFICATION_ID: Int = 1339279
}
