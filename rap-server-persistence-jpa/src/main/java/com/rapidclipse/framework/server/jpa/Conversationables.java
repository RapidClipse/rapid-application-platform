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

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityTransaction;
import jakarta.persistence.RollbackException;

import com.vaadin.flow.internal.CurrentInstance;
import com.vaadin.flow.server.VaadinSession;
import com.vaadin.flow.server.VaadinSessionState;


/**
 * @author XDEV Software
 */
public interface Conversationables extends Serializable
{
	public Conversationables put(String persistenceUnit, Conversationable conversation);

	public Conversationable remove(String persistenceUnit);

	public Conversationable get(String persistenceUnit);

	public void forEach(Consumer<Conversationable> consumer);

	public void closeAll();

	public void close(Conversationable conversationable);

	public static Conversationables getCurrent()
	{
		final Conversationables conversationables = CurrentInstance.get(Conversationables.class);
		if(conversationables != null)
		{
			return conversationables;
		}

		final VaadinSession session = VaadinSession.getCurrent();
		if(session != null && session.getState() == VaadinSessionState.OPEN)
		{
			return session.getAttribute(Conversationables.class);
		}

		return null;
	}

	public static Conversationables New()
	{
		return new Default();
	}

	public static class Default implements Conversationables
	{
		private transient Map<String, Conversationable> unitToConversationable;

		protected Default()
		{
			super();
		}

		private Map<String, Conversationable> unitToConversationable()
		{
			if(this.unitToConversationable == null)
			{
				this.unitToConversationable = new LinkedHashMap<>();
			}
			return this.unitToConversationable;
		}

		@Override
		public Conversationables put(
			final String persistenceUnit,
			final Conversationable conversation)
		{
			unitToConversationable().put(persistenceUnit, conversation);
			return this;
		}

		@Override
		public Conversationable remove(final String persistenceUnit)
		{
			return unitToConversationable().remove(persistenceUnit);
		}

		@Override
		public Conversationable get(final String persistenceUnit)
		{
			return unitToConversationable().get(persistenceUnit);
		}

		@Override
		public void forEach(final Consumer<Conversationable> consumer)
		{
			unitToConversationable().values().forEach(consumer);
		}

		@Override
		public void closeAll()
		{
			unitToConversationable().values().forEach(this::close);
		}

		@Override
		public void close(final Conversationable conversationable)
		{
			final EntityManager em = conversationable.getEntityManager();
			if(em != null && em.isOpen())
			{
				final EntityTransaction transaction = em.getTransaction();
				if(transaction != null && transaction.isActive())
				{
					try
					{
						transaction.commit();
					}
					catch(final RollbackException e)
					{
						if(transaction.isActive())
						{
							transaction.rollback();
						}
					}
				}

				try
				{
					em.close();
				}
				catch(final Exception e)
				{
					if(transaction != null && transaction.isActive())
					{
						transaction.rollback();
					}
				}
			}
		}
	}
}
