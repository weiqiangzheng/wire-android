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
package com.waz.zclient.appentry.controllers

import com.waz.utils.events.EventContext
import com.waz.zclient.{Injectable, Injector}

class CreateTeamController(implicit inj: Injector, eventContext: EventContext) extends Injectable {

  //Vars to persist text in edit boxes
  var teamName = ""
  var teamEmail = ""
  var code = ""
  var teamUserName = ""
  var teamUserUsername = ""
  var password = ""

  def clearCredentials(): Unit = {
    teamName = ""
    teamEmail = ""
    code = ""
    teamUserName = ""
    teamUserUsername = ""
    password = ""
  }

  def goTo(): Unit = {

  }
}
/*
  object SetTeamEmail            extends AppEntryStage { override val depth = 2 }
  object VerifyTeamEmail         extends AppEntryStage { override val depth = 3 }
  object SetUsersNameTeam        extends AppEntryStage { override val depth = 4 }
  object SetPasswordTeam         extends AppEntryStage { override val depth = 5 }
  object SetUsernameTeam         extends AppEntryStage { override val depth = 6 }
  object TeamSetPicture          extends AppEntryStage { override val depth = 6 }
  object InviteToTeam            extends AppEntryStage { override val depth = 7 }
 */
