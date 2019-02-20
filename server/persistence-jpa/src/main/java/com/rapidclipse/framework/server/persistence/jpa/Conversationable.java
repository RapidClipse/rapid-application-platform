
package com.rapidclipse.framework.server.persistence.jpa;

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
	
	public class Implementation implements Conversationable, Serializable
	{
		private EntityManager entityManager;
		private Conversation  conversation;
		
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
