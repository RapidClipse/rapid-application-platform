/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
