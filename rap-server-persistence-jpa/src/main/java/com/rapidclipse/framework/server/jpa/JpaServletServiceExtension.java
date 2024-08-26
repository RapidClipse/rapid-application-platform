/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa;

import com.rapidclipse.framework.server.RapServletService;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public class JpaServletServiceExtension implements RapServletService.Extension
{
	public JpaServletServiceExtension()
	{
		super();
	}
	
	@Override
	public void sessionCreated(
		final RapServletService service,
		final VaadinSession session,
		final VaadinRequest request)
	{
		session.setAttribute(Conversationables.class, Conversationables.New());
	}
	
	@Override
	public void onRequestStart(final RapServletService service, final VaadinSession session)
	{
		final Conversationables conversationables = session.getAttribute(Conversationables.class);
		if(conversationables != null)
		{
			for(final String persistenceUnit : Jpa.getPersistenceManager().getPersistenceUnits())
			{
				Jpa.getSessionStrategyProvider()
					.getRequestStartSessionStrategy(conversationables, persistenceUnit)
					.requestStart(conversationables, persistenceUnit);
			}
		}
	}
	
	@Override
	public void onRequestEnd(final RapServletService service, final VaadinSession session)
	{
		final SessionStrategyProvider sessionStrategyProvider = Jpa.getSessionStrategyProvider();
		if(session != null && sessionStrategyProvider != null)
		{
			final Conversationables conversationables = session
				.getAttribute(Conversationables.class);
			if(conversationables != null)
			{
				for(final String persistenceUnit : Jpa.getPersistenceManager()
					.getPersistenceUnits())
				{
					sessionStrategyProvider
						.getRequestEndSessionStrategy(conversationables, persistenceUnit)
						.requestEnd(conversationables, persistenceUnit);
				}
			}
		}
	}
}
