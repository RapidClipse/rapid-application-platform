/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.security.authorization;

import com.vaadin.flow.router.BeforeEvent;


/**
 * Handler which gets called when an unauthorized navigation request happens.
 *
 * @author XDEV Software
 */
@FunctionalInterface
public interface UnauthorizedNavigationRequestHandler
{
	/**
	 * Handles the unauthorized request, e.g. shows an error message and
	 * redirects.
	 *
	 * @param event
	 */
	public void handle(BeforeEvent event);
	
	public static UnauthorizedNavigationRequestHandler Default()
	{
		return new Default();
	}
	
	public static class Default implements UnauthorizedNavigationRequestHandler
	{
		@Override
		public void handle(final BeforeEvent event)
		{
			Authorization.rerouteToPermissionDeniedView(event);
		}
	}
}
