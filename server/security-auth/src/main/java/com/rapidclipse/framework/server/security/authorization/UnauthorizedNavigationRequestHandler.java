/*-
 * ---
 * Rapid Application Platform / Server / Security / Authentication and Authorization
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
	 * @param navigator
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
