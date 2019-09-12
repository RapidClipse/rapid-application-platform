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
