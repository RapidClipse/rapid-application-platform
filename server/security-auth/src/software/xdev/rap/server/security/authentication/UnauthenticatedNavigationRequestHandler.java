/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.security.authentication;


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
	 * @param navigator
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