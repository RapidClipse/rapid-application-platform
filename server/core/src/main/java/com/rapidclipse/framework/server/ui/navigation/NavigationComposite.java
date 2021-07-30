/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.navigation;

import static java.util.Objects.requireNonNull;

import java.beans.Beans;

import com.rapidclipse.framework.server.navigation.NavigationItemProvider;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasStyle;


/**
 * @author XDEV Software
 *
 */
public abstract class NavigationComposite<T extends Component>
	extends Composite<T>
	implements HasSize, HasStyle
{
	private NavigationItemProvider itemProvider;
	
	public NavigationComposite()
	{
		super();

		if(!Beans.isDesignTime())
		{
			addAttachListener(event -> updateContent());
		}
	}
	
	public void setItemProvider(final NavigationItemProvider itemProvider)
	{
		this.itemProvider = requireNonNull(itemProvider);
	}
	
	public NavigationItemProvider getItemProvider()
	{
		return this.itemProvider != null
			? this.itemProvider
			: (this.itemProvider = NavigationItemProvider.New());
	}
	
	protected abstract void updateContent();
}
