/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
