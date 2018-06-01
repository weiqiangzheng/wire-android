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

import java.util.concurrent.TimeUnit

import android.content.Context
import com.waz.api.IConversation
import com.waz.api.NotificationsHandler.NotificationType
import com.waz.api.impl.AccentColor
import com.waz.content._
import com.waz.model._
import com.waz.service.conversation.{ConversationsListStateService, ConversationsService, ConversationsUiService}
import com.waz.service.images.ImageLoader
import com.waz.service.push.NotificationService.NotificationInfo
import com.waz.service.push.{GlobalNotificationsService, NotificationService}
import com.waz.service.{AccountsService, UiLifeCycle, UserService}
import com.waz.specs.AndroidFreeSpec
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.returning
import com.waz.zclient.WireApplication.{AccountToAssetsStorage, AccountToImageLoader}
import com.waz.zclient.common.controllers.SoundController
import com.waz.zclient.common.controllers.global.AccentColorController
import com.waz.zclient.controllers.navigation.INavigationController
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.messages.controllers.NavigationController
import com.waz.zclient.utils.ResString
import com.waz.zclient.utils.ResString.{AnyRefArgs, StringArgs}
import com.waz.zclient.{Module, WireContext}
import org.junit.Test
import org.scalatest.Suite
import org.threeten.bp.Instant

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class MessageNotificationsControllerTest extends AndroidFreeSpec { this: Suite =>
  import com.waz.ZLog.ImplicitTag.implicitLogTag

  implicit val context: WireContext = null

  implicit val timeout = Duration(5000, TimeUnit.MILLISECONDS)

  val userData = UserData("TestUser")
  val userId = userData.id
  val convId = ConvId(userId.str)
  val convData = ConversationData(convId, RConvId(), None, userId, IConversation.Type.ONE_TO_ONE)

  // a utility method for sending infos from tests
  def sendOne(info: NotificationInfo, userId: UserId = userId) = {
    globalNotifications.groupedNotifications ! Map(userId -> (true, Seq(info)))
  }

  def send(ns: Seq[NotificationInfo], userId: UserId = userId) = {
    globalNotifications.groupedNotifications ! Map(userId -> (true, ns))
  }

  val displayedNots = new SourceSignal[Map[UserId, Seq[NotId]]]()

  val globalNotifications = new GlobalNotificationsService {
    override val groupedNotifications = Signal(Map.empty[UserId, (Boolean, Seq[NotificationService.NotificationInfo])])

    override def markAsDisplayed(userId: UserId, nots: Seq[NotId]): Future[Any] =
      Future.successful(displayedNots.mutate { cur =>
        cur + (userId -> (cur.getOrElse(userId, Seq.empty).toSet ++ nots).toSeq)
      })
  }

  val notsInManager = new SourceSignal[Map[Int, NotificationProps]]()

  class NewModule extends Module {

    private val notificationManager = new NotificationManagerWrapper {

      override def cancel(id: Int): Unit = {
        notsInManager ! notsInManager.currentValue.getOrElse(Map.empty).filterKeys(_ != id)
      }

      override def getActiveNotificationIds: Seq[Int] = {
        notsInManager.currentValue.getOrElse(Map.empty).keys.toSeq
      }

      override def notify(id: Int, props: NotificationProps)(implicit ctx: Context): Unit = {
        notsInManager ! (notsInManager.currentValue.getOrElse(Map.empty) + (id -> props))
      }
    }

    private val uiLifeCycle = returning(mock[UiLifeCycle]) { uiLifeCycle =>
      (uiLifeCycle.uiActive _).expects().anyNumberOfTimes().returning(Signal.const(true))
    }

    private val usersStorage = returning(mock[UsersStorage]) { usersStorage =>
      (usersStorage.signal _).expects(userId).anyNumberOfTimes().returning(Signal.const(userData))
      (usersStorage.optSignal _).expects(userId).anyNumberOfTimes().returning(Signal.const(Some(userData)))
    }

    private val convStorage = returning(mock[ConversationStorage]) { convStorage =>
      (convStorage.convsSignal _).expects().anyNumberOfTimes().returning(Signal(ConversationsSet(SortedSet(convData))))
      (convStorage.optSignal _).expects(convId).anyNumberOfTimes().returning(Signal.const(Option(convData)))
      (convStorage.conversations _).expects().anyNumberOfTimes().returning(IndexedSeq(convData))
    }

    private val convsStats = returning(mock[ConversationsListStateService]) { convsStats =>
      (convsStats.selectedConversationId _).expects().anyNumberOfTimes().returning(Signal.const(Some(convId)))
      (convsStats.selectConversation _).expects(Some(convId)).anyNumberOfTimes().returning(Future.successful(()))
    }

    private val convsUi = returning(mock[ConversationsUiService]) { convsUi =>
      (convsUi.setConversationArchived _).expects(convId, false).anyNumberOfTimes().returning(Future.successful(Option(convData)))
    }

    private val conversations = returning(mock[ConversationsService]) { conversations =>
      (conversations.forceNameUpdate _).expects(convId).anyNumberOfTimes().returning(Future.successful(Option((convData, convData))))
      (conversations.isGroupConversation _).expects(convId).anyNumberOfTimes().returning(Future.successful(false))
    }

    private val imageLoader = mock[ImageLoader]

    private val assetsStorage = mock[AssetsStorage]
    (assetsStorage.get _).expects(*).anyNumberOfTimes().returning(Future.successful(Option.empty[AssetData]))

    private val iNavController = returning(mock[INavigationController]) { iNavController =>
      (iNavController.addNavigationControllerObserver _).expects(*).anyNumberOfTimes()
    }

    private val soundController = returning(mock[SoundController]) { ctrl =>
      (ctrl.isVibrationEnabled _).expects().anyNumberOfTimes().returning(false)
      (ctrl.soundIntensityFull _).expects().anyNumberOfTimes().returning(false)
      (ctrl.soundIntensityNone _).expects().anyNumberOfTimes().returning(true)
    }

    // `MessageNotificationController` receives `NotificationInfo`s from here
    bind[GlobalNotificationsService] to globalNotifications
    // processed notifications end up here
    bind[NotificationManagerWrapper] to notificationManager

    // mocked global entities
    bind[TeamsStorage]    to mock[TeamsStorage]
    bind[AccountsService] to accounts
    bind[UiLifeCycle]     to uiLifeCycle

    // mocked services of the current ZMessaging
    bind[Signal[UserId]]                        to Signal.const(userId)
    bind[Signal[Option[UserId]]]                to Signal.const(Some(userId))
    bind[Signal[AccentColor]]                   to Signal.const(AccentColor(0, 0, 0, 0))
    bind[Signal[UsersStorage]]                  to Signal.const(usersStorage)
    bind[Signal[ConversationStorage]]           to Signal.const(convStorage)
    bind[Signal[ConversationsListStateService]] to Signal.const(convsStats)
    bind[Signal[ConversationsUiService]]        to Signal.const(mock[ConversationsUiService])
    bind[Signal[ConversationsService]]          to Signal.const(conversations)
    bind[Signal[MembersStorage]]                to Signal.const(mock[MembersStorage])
    bind[Signal[UserService]]                   to Signal.const(mock[UserService])
    bind[Signal[OtrClientsStorage]]             to Signal.const(mock[OtrClientsStorage])
    bind[Signal[AssetsStorage]]                 to Signal.const(mock[AssetsStorage])
    bind[Signal[ImageLoader]]                   to Signal.const(imageLoader)

    bind[Signal[AccountsService]] to Signal.const(accounts)
    (accounts.accountsWithManagers _).expects().anyNumberOfTimes().returning(Signal.const(Set(userId)))

    bind [AccountToImageLoader]   to (_ => Future.successful(Option(imageLoader)))
    bind [AccountToAssetsStorage] to (_ => Future.successful(Option(assetsStorage)))

    // mocked controllers
    bind[AccentColorController]  to new AccentColorController()
    bind[NavigationController]   to new NavigationController()
    bind[INavigationController]  to iNavController

    private val convController = new ConversationController()
    convController.selectConv(Some(convId), ConversationChangeRequester.START_CONVERSATION)
    bind[ConversationController] to convController
    bind[SoundController]        to soundController
  }

  private def waitFor(filter: Map[Int, NotificationProps] => Boolean) =
    Await.result(notsInManager.filter(filter).head, timeout)

  // TODO: check why 'beforeAll' is not called automatically
  private def setup(bundleEnabled: Boolean = false): Unit = {
    this.beforeAll()
    implicit val module: NewModule = new NewModule

    notsInManager ! Map.empty
    waitFor(_.isEmpty)

    new MessageNotificationsController(bundleEnabled = bundleEnabled, applicationId = "")
  }

  @Test
  def displayNotificationForReceivedLike(): Unit = {
    setup()

    sendOne(NotificationInfo(NotId(Uid().str), NotificationType.LIKE, Instant.now, "", convId) )

    waitFor(_.size == 1)
  }

  @Test
  def receiveConnectRequest(): Unit = {
    setup()

    sendOne(
      NotificationInfo(
        NotId(Uid().str),
        NotificationType.CONNECT_REQUEST,
        Instant.now(),
        "",
        convId,
        Some(userData.name),
        Some(userData.name),
        userPicture = None,
        isEphemeral = false,
        hasBeenDisplayed = false
      )
    )

    waitFor(_.size == 1)
  }

  @Test
  def receiveTextNotification(): Unit = {
    setup(bundleEnabled = true)

    val info =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "1:1", ConvId(), Some("user1"),
        Some("user1"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    sendOne(info)

    waitFor(_.size == 1)
  }

  @Test
  def receiveTwoTextsFromOneUser(): Unit = {
    setup()

    val convId = ConvId()

    val info1 =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "1:1", convId, Some("user1"),
        Some("user1"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    val info2 =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "1:2", convId, Some("user1"),
        Some("user1"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    send(List(info1, info2))

    waitFor(_.size == 1)
  }

  @Test
  def receiveTwoTextsFromTwoUsers(): Unit = {
    setup()

    val convId1 = ConvId()
    val convId2 = ConvId()

    val info1 =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "one", convId1, Some("user1"),
        Some("user1"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    val info2 =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "two", convId2, Some("user2"),
        Some("user2"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    send(List(info1, info2))

    waitFor { nots =>
      nots.size == 1  && // just one notification for multiple messages
      nots.head._2.contentTitle.exists(_.header.args == AnyRefArgs(List("2"))) && // the title says there are two conversations
      nots.head._2.contentText.exists(_.body == ResString(0, 0, StringArgs(List("two")))) // the main message is the last one
    }

  }

  @Test
  def receiveNotificationsFromManyUsers(): Unit = {
    setup()

    val user1 = (UserId(), "user1")
    val user2 = (UserId(), "user2")
    val user3 = (UserId(), "user3")

    val ns = List(
      (user1, "1:1"),
      (user2, "2:2"),
      (user3, "3:3"),
      (user1, "1:4"),
      (user3, "3:5"),
      (user2, "2:6"),
      (user3, "3:7"),
      (user1, "1:8"),
      (user3, "3:9")
    )

    val infos = ns.map { n =>
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), n._2, ConvId(n._1._1.str), Some(n._1._2),
        Some(n._1._2), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )
    }

    send(infos, userId)

    waitFor { nots =>
      nots.size == 1 &&
      nots.head._2.contentTitle.exists(_.header.args == AnyRefArgs(List("3"))) && // three conversations
      nots.head._2.contentText.exists(_.body == ResString(0, 0, StringArgs(List("3:9")))) // the main message is the last one
    }
  }

  @Test
  def receiveTwoTextsFromOneUserInBundle(): Unit = {
    setup(bundleEnabled = true)

    val convId = ConvId()

    val info1 =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "1:1", convId, Some("user1"),
        Some("user1"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    val info2 =
      NotificationInfo(
        NotId(Uid().str), NotificationType.TEXT, Instant.now(), "1:2", convId, Some("user1"),
        Some("user1"), userPicture = None, isEphemeral = false, hasBeenDisplayed = false
      )

    send(List(info1, info2))

    waitFor { nots =>
      nots.size == 1 &&
      nots.head._2.groupSummary.contains(true) &&
      nots.head._2.group.contains(userId)
    }

    waitFor { nots =>
      nots.size == 2 &&
      nots.tail.head._2.group.contains(userId)
    }
  }

}
