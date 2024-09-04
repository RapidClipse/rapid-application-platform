/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
