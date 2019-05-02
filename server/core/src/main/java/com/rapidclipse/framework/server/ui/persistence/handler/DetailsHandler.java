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

package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.details.Details;


public class DetailsHandler extends ComponentHandler<Details>
{
	protected static final String OPENED = "opened";

	@Override
	public Class<Details> handledType()
	{
		return Details.class;
	}

	@Override
	protected void addEntryValues(final Map<String, Object> entryValues, final Details component)
	{
		super.addEntryValues(entryValues, component);

		entryValues.put(OPENED, component.isOpened());
	}

	@Override
	public void restore(final Details component, final GuiPersistenceEntry entry)
	{
		super.restore(component, entry);

		component.setOpened(((Boolean)entry.value(OPENED)).booleanValue());
	}
}
