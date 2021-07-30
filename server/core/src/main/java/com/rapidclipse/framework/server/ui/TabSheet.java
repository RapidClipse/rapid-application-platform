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
package com.rapidclipse.framework.server.ui;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import com.vaadin.flow.component.tabs.Tabs.Orientation;
import com.vaadin.flow.component.tabs.Tabs.SelectedChangeEvent;
import com.vaadin.flow.shared.Registration;


/**
 * A composite which provides the possibility to add content to {@link Tab}s in a {@link Tabs} component.
 *
 * @author XDEV Software
 *
 */
public class TabSheet extends Composite<FlexLayout>
{
	private final Tabs                tabs;
	private final FlexLayout          tabContentLayout;
	private final Map<Tab, Component> components;
	
	public TabSheet()
	{
		super();
		
		this.tabs             = createTabs();
		this.tabContentLayout = createTabContentLayout();
		this.components       = new LinkedHashMap<>();
		
		final FlexLayout content = getContent();
		content.add(this.tabs);
		content.add(this.tabContentLayout);
		content.setWidth("100%");
		
		content.getStyle().set("flex-direction", "column");
		content.setFlexGrow(1, this.tabContentLayout);
		this.tabs.getStyle().set("flex-shrink", "0.0");
		
		this.tabs.addSelectedChangeListener(event -> {
			final Tab selectedTab = event.getSource().getSelectedTab();
			this.tabContentLayout.removeAll();
			Optional.ofNullable(this.components.get(selectedTab)).ifPresent(this.tabContentLayout::add);
		});
	}
	
	protected Tabs createTabs()
	{
		return new Tabs();
	}
	
	protected FlexLayout createTabContentLayout()
	{
		return new FlexLayout();
	}
	
	public Registration addSelectedChangeListener(final ComponentEventListener<SelectedChangeEvent> listener)
	{
		return this.tabs.addSelectedChangeListener(listener);
	}
	
	public int getSelectedIndex()
	{
		return this.tabs.getSelectedIndex();
	}
	
	public void setSelectedIndex(final int selectedIndex)
	{
		this.tabs.setSelectedIndex(selectedIndex);
	}
	
	public Tab getSelectedTab()
	{
		return this.tabs.getSelectedTab();
	}
	
	public void setSelectedTab(final Tab selectedTab)
	{
		this.tabs.setSelectedTab(selectedTab);
	}
	
	public Component getSelectedComponent()
	{
		final Tab selectedTab = getSelectedTab();
		return selectedTab != null
			? this.components.get(selectedTab)
			: null;
	}
	
	public void setSelectedComponent(final Component component)
	{
		setSelectedTab(getTab(component));
	}
	
	public Tab addTab(final String label, final Component component)
	{
		return addTab(new Tab(label), component);
	}

	public Tab addTab(final Component tab, final Component component)
	{
		return addTab(new Tab(tab), component);
	}
	
	protected Tab addTab(final Tab tab, final Component component)
	{
		this.tabs.add(tab);
		
		if(this.components.isEmpty())
		{
			this.tabContentLayout.add(component);
		}
		this.components.put(tab, component);
		
		return tab;
	}
	
	public void removeTab(final Tab tab)
	{
		if(this.tabs.getSelectedTab() == tab)
		{
			final int index = this.tabs.getSelectedIndex();
			this.tabs.setSelectedIndex(Math.max(0, index - 1));
		}
		this.tabs.remove(tab);
		Optional.ofNullable(this.components.get(tab)).ifPresent(this.tabContentLayout::remove);
	}
	
	public Orientation getOrientation()
	{
		return this.tabs.getOrientation();
	}
	
	public void setOrientation(final Orientation orientation)
	{
		this.tabs.setOrientation(orientation);
		
		getContent().getStyle().set("flex-direction",
			orientation == Orientation.HORIZONTAL
				? "column"
				: "row");
	}
	
	/**
	 * @see Tabs#setFlexGrowForEnclosedTabs(double)
	 * @param flexGrow
	 */
	public void setFlexGrowForEnclosedTabs(final double flexGrow)
	{
		this.tabs.setFlexGrowForEnclosedTabs(flexGrow);
	}
	
	public Iterable<Tab> tabs()
	{
		return this.components.keySet();
	}
	
	public Component getComponent(final Tab tab)
	{
		return this.components.get(tab);
	}
	
	public Tab getTab(final Component component)
	{
		return this.components.entrySet().stream()
			.filter(entry -> entry.getValue() == component)
			.map(entry -> entry.getKey())
			.findAny()
			.orElse(null);
	}
}
