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

package com.rapidclipse.framework.server.concurrent;

import com.rapidclipse.framework.server.persistence.jpa.Conversationables;
import com.rapidclipse.framework.server.persistence.jpa.Jpa;
import com.rapidclipse.framework.server.persistence.jpa.PersistenceManager;
import com.rapidclipse.framework.server.persistence.jpa.SessionStrategyProvider;
import com.vaadin.flow.internal.CurrentInstance;


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
		
		final Conversationables  conversationables  = Conversationables.New();
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
