/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.security;

import java.beans.Beans;

import com.rapidclipse.framework.server.navigation.AuthNavigationListener;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;


/**
 * @author XDEV Software
 *
 */
public class AuthServiceInitListener implements VaadinServiceInitListener
{
	public AuthServiceInitListener()
	{
		super();
	}

	@Override
	public void serviceInit(final ServiceInitEvent event)
	{
		if(Beans.isDesignTime())
		{
			/*
			 * Controller in design time causes unwanted routing.
			 */
			return;
		}

		event.getSource().addUIInitListener(
			e -> e.getUI().addBeforeEnterListener(new AuthNavigationListener()));
	}
}
