/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.security;

import java.beans.Beans;

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
			e -> e.getUI().addBeforeEnterListener(new AuthNavigationController()));
	}
}
