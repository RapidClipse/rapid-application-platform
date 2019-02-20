
package com.rapidclipse.framework.server.persistence.jpa;

import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityTransaction;
import javax.persistence.LockModeType;


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
			final Conversation conversation = new Conversation.Implementation();
			
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
