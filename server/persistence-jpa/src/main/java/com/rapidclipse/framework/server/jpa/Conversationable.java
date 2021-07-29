/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.jpa;

import java.io.Serializable;
import java.util.function.Supplier;

import javax.persistence.EntityManager;


/**
 * @author XDEV Software
 *
 */
public interface Conversationable
{
	public EntityManager getEntityManager();
	
	public EntityManager peekEntityManager();

	public void setConversation(Conversation conversation);

	public Conversation getConversation();

	public static Conversationable New(final Supplier<EntityManager> entityManagerSupplier)
	{
		return new Default(entityManagerSupplier);
	}

	public class Default implements Conversationable, Serializable
	{
		private final Supplier<EntityManager> entityManagerSupplier;
		private volatile EntityManager        entityManager;
		private Conversation                  conversation;

		protected Default(final Supplier<EntityManager> entityManagerSupplier)
		{
			super();
			this.entityManagerSupplier = entityManagerSupplier;
		}

		@Override
		public EntityManager getEntityManager()
		{
			EntityManager entityManager;
			if((entityManager = this.entityManager) == null)
			{
				synchronized(this)
				{
					if((entityManager = this.entityManager) == null)
					{
						entityManager = this.entityManager = this.entityManagerSupplier.get();
					}
				}
			}
			return entityManager;
		}
		
		@Override
		public EntityManager peekEntityManager()
		{
			return this.entityManager;
		}

		@Override
		public synchronized void setConversation(final Conversation conversation)
		{
			if(this.conversation != null && this.conversation.isActive())
			{
				throw new RuntimeException(
					"Another conversation is already running. Only one active conversation is allowed per conversationable.");
			}

			this.conversation = conversation;
		}

		@Override
		public synchronized Conversation getConversation()
		{
			return this.conversation;
		}
	}
}
