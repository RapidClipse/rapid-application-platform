/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
