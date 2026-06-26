/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import com.vaadin.flow.component.ComponentEvent;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public class SelectionEvent<T extends AbstractChart> extends ComponentEvent<T>
{
	private final Selection selection;
	
	SelectionEvent(final T source, final boolean fromClient, final Selection selection)
	{
		super(source, fromClient);

		this.selection = selection;
	}
	
	public Selection getSelection()
	{
		return this.selection;
	}
}
