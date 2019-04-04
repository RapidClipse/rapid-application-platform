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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

import javax.persistence.EntityManager;
import javax.persistence.EntityTransaction;
import javax.persistence.RollbackException;

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
		return new Implementation();
	}
	
	public static class Implementation implements Conversationables
	{
		private transient Map<String, Conversationable> unitToConversationable;
		
		public Implementation()
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
