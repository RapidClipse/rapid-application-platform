/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
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
