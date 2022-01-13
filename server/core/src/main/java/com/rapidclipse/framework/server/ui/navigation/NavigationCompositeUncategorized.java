/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.navigation;

import static java.util.Objects.requireNonNull;

import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;


/**
 * @author XDEV Software
 *
 */
public abstract class NavigationCompositeUncategorized<T extends Component & HasComponents>
	extends NavigationComposite<T>
{
	private NavigationItemComponentFactory itemComponentFactory;

	protected NavigationCompositeUncategorized()
	{
		super();
	}

	public void setItemComponentFactory(final NavigationItemComponentFactory itemComponentFactory)
	{
		this.itemComponentFactory = requireNonNull(itemComponentFactory);
	}
	
	public NavigationItemComponentFactory getItemComponentFactory()
	{
		return this.itemComponentFactory != null
			? this.itemComponentFactory
			: (this.itemComponentFactory = NavigationItemComponentFactory.LinkFactory());
	}

	@Override
	protected void updateContent()
	{
		final T content = getContent();
		content.removeAll();

		final NavigationItemComponentFactory itemComponentFactory = getItemComponentFactory();

		for(final NavigationItem item : getItemProvider().getItems())
		{
			content.add(itemComponentFactory.apply(item));
		}
	}
}
