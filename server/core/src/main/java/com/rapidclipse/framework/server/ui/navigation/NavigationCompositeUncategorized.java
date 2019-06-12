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
		if(this.itemComponentFactory == null)
		{
			this.itemComponentFactory = NavigationItemComponentFactory.LinkFactory();
		}

		return this.itemComponentFactory;
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
