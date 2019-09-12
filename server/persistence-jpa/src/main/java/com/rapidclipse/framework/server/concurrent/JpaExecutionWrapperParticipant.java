/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.concurrent;

import com.rapidclipse.framework.server.jpa.Conversationables;
import com.rapidclipse.framework.server.jpa.Jpa;
import com.rapidclipse.framework.server.jpa.PersistenceManager;
import com.rapidclipse.framework.server.jpa.SessionStrategyProvider;
import com.vaadin.flow.internal.CurrentInstance;


/**
 * @author XDEV Software
 *
 */
public class JpaExecutionWrapperParticipant implements ExecutionWrapper.Participant
{
	public JpaExecutionWrapperParticipant()
	{
		super();
	}
	
	@Override
	public void before()
	{
		final PersistenceManager persistenceManager = Jpa.getPersistenceManager();
		
		final Conversationables conversationables = Conversationables.New();
		CurrentInstance.set(Conversationables.class, conversationables);
		
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();
		
		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			sessionStrategyProvider
				.getRequestStartSessionStrategy(conversationables, persistenceUnit)
				.requestStart(conversationables, persistenceUnit);
		}
	}
	
	@Override
	public void after()
	{
		final PersistenceManager      persistenceManager      = Jpa.getPersistenceManager();
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();
		final Conversationables       conversationables       = CurrentInstance.get(Conversationables.class);
		
		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			sessionStrategyProvider.getRequestEndSessionStrategy(conversationables, persistenceUnit)
				.requestEnd(conversationables, persistenceUnit);
		}
		
		CurrentInstance.set(Conversationables.class, null);
	}
}
