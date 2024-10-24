/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa;

import java.util.Map;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityTransaction;
import jakarta.persistence.LockModeType;


/**
 * @author XDEV Software
 *
 */
public final class ConversationUtils
{
	public static Conversation getConversation()
	{
		return getConversation(Jpa.getPersistenceManager().getDefaultPersistenceUnit());
	}

	public static Conversation getConversation(final String persistenceUnit)
	{
		final Conversationable conversationable = Conversationables.getCurrent()
			.get(persistenceUnit);
		return conversationable != null ? conversationable.getConversation() : null;
	}

	private static Conversation newConversation(final String persistenceUnit)
	{
		final Conversationable conversationable = Conversationables.getCurrent()
			.get(persistenceUnit);
		if(conversationable != null)
		{
			final Conversation conversation = Conversation.New();

			try
			{
				conversationable.setConversation(conversation);
			}
			catch(final RuntimeException e)
			{
				return conversationable.getConversation();
			}

			return conversation;
		}

		return null;
	}

	public static Conversation startConversation()
	{
		return startConversation(Jpa.getPersistenceManager().getDefaultPersistenceUnit());
	}

	public static Conversation startConversation(final String persistenceUnit)
	{
		final Conversation conversation = newConversation(persistenceUnit);
		if(conversation != null)
		{
			conversation.start();
		}
		return conversation;
	}

	public static void startPessimisticConversation()
	{
		startPessimisticConversation(LockModeType.WRITE);
	}

	public static void startPessimisticConversation(final String persistenceUnit)
	{
		startPessimisticConversation(persistenceUnit, LockModeType.WRITE);
	}

	public static Conversation startPessimisticConversation(final LockModeType lockMode)
	{
		return startPessimisticConversation(Jpa.getPersistenceManager().getDefaultPersistenceUnit(),
			lockMode);
	}

	public static Conversation startPessimisticConversation(
		final String persistenceUnit,
		final LockModeType lockMode)
	{
		final Conversation conversation = newConversation(persistenceUnit);
		if(conversation != null)
		{
			conversation.setPessimisticUnit(true, lockMode);
			conversation.start();
		}
		return conversation;
	}

	public static void lockConversation(final Object entity)
	{
		final String           persistenceUnit  = Jpa.getPersistenceManager()
			.getPersistenceUnit(entity.getClass());
		final Conversationable conversationable = Conversationables.getCurrent()
			.get(persistenceUnit);
		if(conversationable != null)
		{
			final Conversation conversation = conversationable.getConversation();
			if(conversation != null)
			{
				conversationable.getEntityManager().lock(entity, conversation.getLockModeType());
			}
		}
	}

	public static void lockConversation(final Object entity, final Map<String, Object> properties)
	{
		final String           persistenceUnit  = Jpa.getPersistenceManager()
			.getPersistenceUnit(entity.getClass());
		final Conversationable conversationable = Conversationables.getCurrent()
			.get(persistenceUnit);
		if(conversationable != null)
		{
			final Conversation conversation = conversationable.getConversation();
			if(conversation != null)
			{
				conversationable.getEntityManager().lock(entity, conversation.getLockModeType(),
					properties);
			}
		}
	}

	public static void releaseConversationLock()
	{
		releaseConversationLock(Jpa.getPersistenceManager().getDefaultPersistenceUnit());
	}

	public static void releaseConversationLock(final String persistenceUnit)
	{
		final EntityManager em = Jpa.getEntityManager(persistenceUnit);
		if(em != null)
		{
			final EntityTransaction transaction = em.getTransaction();
			if(transaction.isActive())
			{
				transaction.commit();
			}
		}
	}

	public static void endConversation()
	{
		final Conversation conversation = getConversation();
		if(conversation != null)
		{
			conversation.end();
		}
	}

	public static void endConversation(final String persistenceUnit)
	{
		final Conversation conversation = getConversation(persistenceUnit);
		if(conversation != null)
		{
			conversation.end();
		}
	}

	public static boolean isConversationActive()
	{
		final Conversation conversation = getConversation();
		if(conversation != null)
		{
			return conversation.isActive();
		}
		return false;
	}

	public static boolean isConversationActive(final String persistenceUnit)
	{
		final Conversation conversation = getConversation(persistenceUnit);
		if(conversation != null)
		{
			return conversation.isActive();
		}
		return false;
	}

	private ConversationUtils()
	{
		throw new Error();
	}
}
