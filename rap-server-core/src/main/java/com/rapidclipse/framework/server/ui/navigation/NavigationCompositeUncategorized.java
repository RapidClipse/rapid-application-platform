/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
