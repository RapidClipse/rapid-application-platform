/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa;

import java.util.HashMap;
import java.util.Map;

import jakarta.servlet.ServletContext;


/**
 * @author XDEV Software
 *
 */
public interface SessionStrategyProvider
{
	public static final String FACTORY_INIT_PARAMETER = "rap.sessionStrategyProvider.factory";

	public static interface Factory
	{
		public SessionStrategyProvider createSessionStrategyProvider(final ServletContext context);
	}
	
	public SessionStrategy getRequestStartSessionStrategy(
		Conversationables conversationables,
		String persistenceUnit);
	
	public SessionStrategy getRequestEndSessionStrategy(
		Conversationables conversationables,
		String persistenceUnit);

	public static SessionStrategyProvider New()
	{
		return new Default();
	}
	
	public class Default implements SessionStrategyProvider
	{
		protected final SessionStrategy              perRequest                 = new SessionStrategy.PerRequest();
		protected final SessionStrategy              perConversation            = new SessionStrategy.PerConversation();
		protected final SessionStrategy              perConversationPessimistic =
			new SessionStrategy.PerConversationPessimistic();
		protected final Map<String, SessionStrategy> currentStrategies          = new HashMap<>();
		
		protected Default()
		{
			super();
		}
		
		protected SessionStrategy storeSessionStrategy(
			final String persistenceUnit,
			final SessionStrategy strategy)
		{
			this.currentStrategies.put(persistenceUnit, strategy);
			return strategy;
		}
		
		@Override
		public SessionStrategy getRequestStartSessionStrategy(
			final Conversationables conversationables,
			final String persistenceUnit)
			throws RuntimeException
		{
			final Conversationable conversationable = conversationables.get(persistenceUnit);
			
			if(conversationable != null)
			{
				if(conversationable.getConversation() != null)
				{
					if(conversationable.getConversation().isActive())
					{
						if(conversationable.getConversation().isPessimisticUnit())
						{
							return storeSessionStrategy(persistenceUnit,
								this.perConversationPessimistic);
						}
						else
						{
							return storeSessionStrategy(persistenceUnit, this.perConversation);
						}
					}
				}
			}
			
			// return default strategy
			return storeSessionStrategy(persistenceUnit, this.perRequest);
		}
		
		@Override
		public SessionStrategy getRequestEndSessionStrategy(
			final Conversationables conversationables,
			final String persistenceUnit)
		{
			/*
			 * end request with existing strategy - don't exchange strategies
			 * between request / response
			 */
			final SessionStrategy strategy = this.currentStrategies.get(persistenceUnit);
			if(strategy != null)
			{
				return strategy;
			}
			
			final Conversationable conversationable = conversationables.get(persistenceUnit);
			
			if(conversationable != null)
			{
				if(conversationable.getConversation() != null)
				{
					if(conversationable.getConversation().isActive())
					{
						if(conversationable.getConversation().isPessimisticUnit())
						{
							return storeSessionStrategy(persistenceUnit,
								this.perConversationPessimistic);
						}
						else
						{
							return storeSessionStrategy(persistenceUnit, this.perConversation);
						}
					}
				}
			}
			
			// return default strategy
			return storeSessionStrategy(persistenceUnit, this.perRequest);
		}
	}
}
