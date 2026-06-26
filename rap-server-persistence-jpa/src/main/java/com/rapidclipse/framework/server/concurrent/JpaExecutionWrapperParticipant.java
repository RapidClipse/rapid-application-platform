/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
