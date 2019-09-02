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
