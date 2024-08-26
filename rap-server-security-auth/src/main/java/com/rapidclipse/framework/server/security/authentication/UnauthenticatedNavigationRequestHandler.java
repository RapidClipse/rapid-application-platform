/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.security.authentication;

import com.vaadin.flow.router.BeforeEvent;


/**
 * Handler which gets called when an unauthenticated navigation request happens,
 * meaning there is no user logged in and the target is no
 * {@link AccessibleView}.
 *
 * @author XDEV Software
 */
@FunctionalInterface
public interface UnauthenticatedNavigationRequestHandler
{
	/**
	 * Handles the unauthenticated request, e.g. shows an error message and
	 * redirects to the login view.
	 *
	 * @param event
	 */
	public void handle(BeforeEvent event);
	
	public static UnauthenticatedNavigationRequestHandler Default()
	{
		return new Default();
	}
	
	public static class Default implements UnauthenticatedNavigationRequestHandler
	{
		@Override
		public void handle(final BeforeEvent event)
		{
			Authentication.rerouteToLoginView(event);
		}
	}
}
