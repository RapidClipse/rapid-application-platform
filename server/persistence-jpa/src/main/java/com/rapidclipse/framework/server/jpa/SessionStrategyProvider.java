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

import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;


/**
 * @author XDEV Software
 *
 */
public interface SessionStrategyProvider
{
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
	
	public static final String FACTORY_INIT_PARAMETER = "rap.sessionStrategyProvider.factory";
	
	public class Implementation implements SessionStrategyProvider
	{
		protected final SessionStrategy              perRequest                 = new SessionStrategy.PerRequest();
		protected final SessionStrategy              perConversation            = new SessionStrategy.PerConversation();
		protected final SessionStrategy              perConversationPessimistic =
			new SessionStrategy.PerConversationPessimistic();
		protected final Map<String, SessionStrategy> currentStrategies          = new HashMap<>();
		
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
