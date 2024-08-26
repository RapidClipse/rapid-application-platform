/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.EntityTransaction;
import jakarta.persistence.RollbackException;

import org.hibernate.FlushMode;
import org.hibernate.Session;


/**
 * Manages Session propagation.
 *
 * @author XDEV Software
 *
 */
public interface SessionStrategy
{
	public void requestStart(Conversationables conversationables, String persistenceUnit);

	public void requestEnd(Conversationables conversationables, String persistenceUnit);

	/**
	 * Request / Response propagation to avoid session per operation anti
	 * pattern.
	 *
	 * @author XDEV Software
	 *
	 */
	public class PerRequest implements SessionStrategy
	{
		@Override
		public void requestStart(
			final Conversationables conversationables,
			final String persistenceUnit)
		{
			final Conversationable conversationable = Conversationable.New(() -> {
				final EntityManagerFactory factory =
					Jpa.getPersistenceManager().getEntityManagerFactory(persistenceUnit);
				final EntityManager        manager = factory.createEntityManager();
				manager.getTransaction().begin();
				return manager;
			});

			conversationables.put(persistenceUnit, conversationable);
		}

		@Override
		public void requestEnd(
			final Conversationables conversationables,
			final String persistenceUnit)
		{
			final Conversationable conversationable = conversationables.get(persistenceUnit);
			if(conversationable != null)
			{
				final EntityManager em = conversationable.peekEntityManager();
				if(em != null)
				{
					final Conversation conversation = conversationable.getConversation();
					if(conversation != null && conversation.isActive())
					{
						/*
						 * Keep the session and with it the persistence context
						 * alive during user think time while a conversation is
						 * active. The next request will automatically be
						 * handled by an appropriate conversation managing
						 * strategy.
						 */

						final EntityTransaction transaction = em.getTransaction();
						if(transaction != null && transaction.isActive())
						{
							try
							{
								// end unit of work
								transaction.commit();
							}
							catch(final RollbackException e)
							{
								transaction.rollback();
							}
						}
					}
					else
					{
						conversationables.close(conversationable);
					}
				}
			}
		}
	}

	/**
	 * Extended persistence context pattern.
	 *
	 * @author XDEV Software
	 *
	 */
	public class PerConversation implements SessionStrategy
	{
		@Override
		public void requestStart(
			final Conversationables conversationables,
			final String persistenceUnit)
		{
			final Conversationable conversationable = conversationables.get(persistenceUnit);
			if(conversationable != null)
			{
				final EntityManager em = conversationable.getEntityManager();
				if(em != null)
				{
					final Session session = em.unwrap(Session.class);
					if(session.getHibernateFlushMode() != FlushMode.MANUAL)
					{
						/*
						 * Prepare conversation - disable AUTO flush mode for
						 * each transaction commit, to achieve the conversation
						 * unit of work context.
						 */
						session.setHibernateFlushMode(FlushMode.MANUAL);
					}

					/*
					 * Begin a database transaction, reconnects Session -
					 * continues the unit of work
					 */
					final EntityTransaction transaction = em.getTransaction();
					if(!transaction.isActive())
					{
						transaction.begin();
					}
				}
			}
		}

		@Override
		public void requestEnd(
			final Conversationables conversationables,
			final String persistenceUnit)
		{
			final Conversationable conversationable = conversationables.get(persistenceUnit);
			if(conversationable != null)
			{
				final Conversation conversation = conversationable.getConversation();
				if(conversation != null)
				{
					final EntityManager em = conversationable.getEntityManager();
					if(em != null)
					{
						final EntityTransaction transaction = em.getTransaction();
						if(conversation.isActive())
						{
							// Event was not the last request, continue
							// conversation
							if(transaction.isActive())
							{
								try
								{
									transaction.commit();
								}
								catch(final RollbackException e)
								{
									transaction.rollback();
								}
							}

						}
						else
						{
							/*
							 * The event was the last request: flush, commit,
							 * close
							 */
							em.flush();
							if(transaction.isActive())
							{
								try
								{
									transaction.commit();
								}
								catch(final RollbackException e)
								{
									transaction.rollback();
								}
							}
							em.close();
						}
					}
				}
			}
		}
	}

	/**
	 * Extended persistence context pattern.
	 *
	 * @author XDEV Software
	 *
	 */
	public class PerConversationPessimistic extends PerConversation
	{
		@Override
		public void requestEnd(
			final Conversationables conversationables,
			final String persistenceUnit)
		{
			final Conversationable conversationable = conversationables.get(persistenceUnit);
			if(conversationable != null)
			{
				final Conversation conversation = conversationable.getConversation();
				if(conversation != null)
				{
					final EntityManager em = conversationable.getEntityManager();
					if(em != null)
					{
						final EntityTransaction transaction = em.getTransaction();
						if(!conversation.isActive())
						{
							/*
							 * The event was the last request: flush, commit,
							 * close
							 */
							em.flush();
							if(transaction.isActive())
							{
								try
								{
									transaction.commit();
								}
								catch(final RollbackException e)
								{
									transaction.rollback();
								}
							}
							em.close();
						}
					}
				}
			}
		}
	}
}
