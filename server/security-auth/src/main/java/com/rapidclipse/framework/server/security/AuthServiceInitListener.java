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

package com.rapidclipse.framework.server.security;

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
		event.getSource().addUIInitListener(
			e -> e.getUI().addBeforeEnterListener(new AuthNavigationController()));
	}
}
