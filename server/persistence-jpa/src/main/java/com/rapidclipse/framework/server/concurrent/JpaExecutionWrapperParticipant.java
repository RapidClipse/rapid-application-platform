/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
	private Conversationables conversationables;

	public JpaExecutionWrapperParticipant()
	{
		super();
	}

	@Override
	public void before()
	{
		CurrentInstance.set(Conversationables.class, this.conversationables = Conversationables.New());

		final PersistenceManager      persistenceManager      = Jpa.getPersistenceManager();
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();

		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			sessionStrategyProvider
				.getRequestStartSessionStrategy(this.conversationables, persistenceUnit)
				.requestStart(this.conversationables, persistenceUnit);
		}
	}

	@Override
	public void after()
	{
		final PersistenceManager      persistenceManager      = Jpa.getPersistenceManager();
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();

		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			sessionStrategyProvider
				.getRequestEndSessionStrategy(this.conversationables, persistenceUnit)
				.requestEnd(this.conversationables, persistenceUnit);
		}

		CurrentInstance.set(Conversationables.class, this.conversationables = null);
	}
}
