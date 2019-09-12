/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa;

import java.io.Serializable;

import javax.persistence.EntityManager;


/**
 * @author XDEV Software
 *
 */
public interface Conversationable
{
	public void setEntityManager(EntityManager em);

	public EntityManager getEntityManager();

	public void setConversation(Conversation conversation);

	public Conversation getConversation();

	public static Conversationable New()
	{
		return new Default();
	}

	public class Default implements Conversationable, Serializable
	{
		private EntityManager entityManager;
		private Conversation  conversation;

		protected Default()
		{
			super();
		}

		@Override
		public void setEntityManager(final EntityManager entityManager)
		{
			this.entityManager = entityManager;
		}

		@Override
		public EntityManager getEntityManager()
		{
			return this.entityManager;
		}

		@Override
		public void setConversation(final Conversation conversation)
		{
			if(this.conversation != null && this.conversation.isActive())
			{
				throw new RuntimeException(
					"Another conversation is already running. Only one active conversation is allowed per conversationable.");
			}

			this.conversation = conversation;
		}

		@Override
		public Conversation getConversation()
		{
			return this.conversation;
		}
	}
}
