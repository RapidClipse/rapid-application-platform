/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.concurrent;


import com.vaadin.flow.internal.CurrentInstance;

import software.xdev.rap.server.persistence.jpa.Conversationables;
import software.xdev.rap.server.persistence.jpa.Jpa;
import software.xdev.rap.server.persistence.jpa.PersistenceManager;
import software.xdev.rap.server.persistence.jpa.SessionStrategyProvider;


/**
 * @author XDEV Software
 *
 */
public class JpaAccessWrapperParticipant implements AccessWrapper.Participant
{
	public JpaAccessWrapperParticipant()
	{
		super();
	}
	
	
	@Override
	public void before()
	{
		final PersistenceManager persistenceManager = Jpa.getPersistenceManager();
		
		final Conversationables conversationables = Conversationables.New();
		CurrentInstance.set(Conversationables.class,conversationables);
		
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();

		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			sessionStrategyProvider
					.getRequestStartSessionStrategy(conversationables,persistenceUnit)
					.requestStart(conversationables,persistenceUnit);
		}
	}
	
	
	@Override
	public void after()
	{
		final PersistenceManager persistenceManager = Jpa.getPersistenceManager();
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();
		final Conversationables conversationables = CurrentInstance.get(Conversationables.class);
		
		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			sessionStrategyProvider.getRequestEndSessionStrategy(conversationables,persistenceUnit)
					.requestEnd(conversationables,persistenceUnit);
		}
		
		CurrentInstance.set(Conversationables.class,null);
	}
}
