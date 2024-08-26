/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.persistence;

import com.vaadin.flow.component.Component;


public interface GuiPersistenceHandler<C extends Component>
{
	public Class<C> handledType();
	
	public GuiPersistenceEntry persist(C component);
	
	public void restore(C component, GuiPersistenceEntry entry);
}
