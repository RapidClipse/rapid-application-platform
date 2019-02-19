/*-
 * ---
 * Rapid Application Platform / Server / Persistence / JPA
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.persistence.jpa;

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
