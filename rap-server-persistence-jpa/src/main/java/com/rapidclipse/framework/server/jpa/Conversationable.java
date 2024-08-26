/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa;

import java.io.Serializable;
import java.util.function.Supplier;

import jakarta.persistence.EntityManager;


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
