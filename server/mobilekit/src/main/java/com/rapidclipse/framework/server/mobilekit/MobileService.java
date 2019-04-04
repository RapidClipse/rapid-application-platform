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
package com.rapidclipse.framework.server.mobilekit;

import com.vaadin.flow.component.UI;


/**
 * @author XDEV Software
 *
 */
public interface MobileService
{
	public static <M extends MobileService> M getCurrent(final Class<M> type)
	{
		return getCurrent(type, UI.getCurrent());
	}
	
	public static <M extends MobileService> M getCurrent(final Class<M> type, final UI ui)
	{
		return ui.getChildren().filter(type::isInstance).map(type::cast).findFirst().orElse(null);
	}
}
