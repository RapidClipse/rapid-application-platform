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

package com.rapidclipse.framework.server.navigation;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;


/**
 * @author XDEV Software
 *
 */
public interface HasNavigationParameters extends HasUrlParameter<String>
{
	@Override
	default void setParameter(final BeforeEvent event, final String parameter)
	{
		NavigationUtils.injectParameters(this, event, parameter);
		navigationParametersUpdated();
	}
	
	default void navigationParametersUpdated()
	{
	}
}
