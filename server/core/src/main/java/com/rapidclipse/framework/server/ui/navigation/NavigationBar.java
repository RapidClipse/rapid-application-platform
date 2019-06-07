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

import java.util.Optional;

import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasOrderedComponents;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.ThemableLayout;
import com.vaadin.flow.function.SerializableFunction;
import com.vaadin.flow.router.RouterLink;


/**
 * @author XDEV Software
 *
 */
public abstract class NavigationBar<T extends Component & ThemableLayout & HasOrderedComponents<T>>
	extends NavigationComposite<T>
{
	public static enum ItemStyle
	{
		LINK(item -> new RouterLink(item.displayName(), item.routeData().getNavigationTarget())),
		
		BUTTON(item -> {
			final Button button = new Button(item.displayName());
			Optional.ofNullable(item.icon()).ifPresent(icon -> button.setIcon(icon.create()));
			button.addClickListener(event -> UI.getCurrent().navigate(item.routeData().getNavigationTarget()));
			return button;
		});

		final SerializableFunction<NavigationItem, Component> componentFactory;
		
		private ItemStyle(final SerializableFunction<NavigationItem, Component> componentFactory)
		{
			this.componentFactory = componentFactory;
		}
	}

	private ItemStyle itemStyle = ItemStyle.LINK;
	
	public NavigationBar()
	{
		super();

		getContent().setSpacing(false);
	}
	
	public void setItemStyle(final ItemStyle itemStyle)
	{
		this.itemStyle = requireNonNull(itemStyle);
	}

	public ItemStyle getItemStyle()
	{
		return this.itemStyle;
	}
	
	@Override
	protected void updateContent()
	{
		final T content = getContent();
		content.removeAll();
		
		for(final NavigationItem item : getItemProvider().getItems())
		{
			content.add(this.itemStyle.componentFactory.apply(item));
		}
	}
}
