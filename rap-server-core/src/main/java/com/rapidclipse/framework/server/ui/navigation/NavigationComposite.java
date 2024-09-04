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
