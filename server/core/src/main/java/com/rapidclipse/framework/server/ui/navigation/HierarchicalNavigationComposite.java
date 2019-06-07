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

import com.vaadin.flow.component.Component;


/**
 * @author XDEV Software
 *
 */
public abstract class HierarchicalNavigationComposite<T extends Component> extends NavigationComposite<T>
{
	private NavigationItemHierarchyProvider hierarchyProvider;

	public HierarchicalNavigationComposite()
	{
		super();
	}
	
	public void setHierarchyProvider(final NavigationItemHierarchyProvider hierarchyProvider)
	{
		this.hierarchyProvider = hierarchyProvider;
	}

	public NavigationItemHierarchyProvider getHierarchyProvider()
	{
		if(this.hierarchyProvider == null)
		{
			this.hierarchyProvider = NavigationItemHierarchyProvider.Plain(getItemProvider().getItems());
		}
		
		return this.hierarchyProvider;
	}
}
