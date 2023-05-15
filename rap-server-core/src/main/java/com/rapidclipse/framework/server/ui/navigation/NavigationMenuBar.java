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

import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.rapidclipse.framework.server.navigation.NavigationElement;
import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.component.contextmenu.HasMenuItems;
import com.vaadin.flow.component.contextmenu.MenuItem;
import com.vaadin.flow.component.contextmenu.SubMenu;
import com.vaadin.flow.component.menubar.MenuBar;
import com.vaadin.flow.function.SerializableBiFunction;


/**
 * @author XDEV Software
 *
 */
public class NavigationMenuBar extends NavigationComposite<MenuBar>
{
	@FunctionalInterface
	public interface MenuItemFactory extends SerializableBiFunction<NavigationItem, HasMenuItems, MenuItem>
	{
		public static MenuItemFactory Default()
		{
			return new Default();
		}

		public static class Default implements MenuItemFactory
		{
			protected Default()
			{
				super();
			}

			@Override
			public MenuItem apply(final NavigationItem item, final HasMenuItems parent)
			{
				final NavigationItemComponentFactory.EventHandler eventHandler =
					NavigationItemComponentFactory.EventHandler.Default();
				return parent.addItem(item.displayName(),
					event -> eventHandler.handleEvent(event.getSource(), item));
			}
		}
	}

	private NavigationHierarchyProvider    hierarchyProvider;
	private NavigationHierarchyLevelSorter hierarchyLevelSorter;
	private MenuItemFactory                menuItemFactory;

	public NavigationMenuBar()
	{
		super();
	}

	public void setHierarchyProvider(final NavigationHierarchyProvider hierarchyProvider)
	{
		this.hierarchyProvider = requireNonNull(hierarchyProvider);
	}

	public NavigationHierarchyProvider getHierarchyProvider()
	{
		return this.hierarchyProvider != null
			? this.hierarchyProvider
			: (this.hierarchyProvider = NavigationHierarchyProvider.ForItems(getItemProvider().getItems()));
	}

	public void setMenuItemFactory(final MenuItemFactory menuItemFactory)
	{
		this.menuItemFactory = requireNonNull(menuItemFactory);
	}

	public MenuItemFactory getMenuItemFactory()
	{
		return this.menuItemFactory != null
			? this.menuItemFactory
			: (this.menuItemFactory = MenuItemFactory.Default());
	}

	public void setHierarchyLevelSorter(final NavigationHierarchyLevelSorter hierarchyLevelSorter)
	{
		this.hierarchyLevelSorter = requireNonNull(hierarchyLevelSorter);
	}

	public NavigationHierarchyLevelSorter getHierarchyLevelSorter()
	{
		return this.hierarchyLevelSorter != null
			? this.hierarchyLevelSorter
			: (this.hierarchyLevelSorter = NavigationHierarchyLevelSorter.Default());
	}

	@Override
	protected void updateContent()
	{
		final MenuBar menuBar = getContent();
		menuBar.removeAll();

		final NavigationHierarchyProvider    hierarchyProvider    = getHierarchyProvider();
		final NavigationHierarchyLevelSorter hierarchyLevelSorter = getHierarchyLevelSorter();
		final List<NavigationItem>           items                = getItemProvider().getItems();
		final MenuItemFactory                menuItemFactory      = getMenuItemFactory();

		createContent(menuBar, hierarchyProvider.getRootCategories(), item -> StringUtils.isEmpty(item.category()),
			items, hierarchyProvider, hierarchyLevelSorter, menuItemFactory);
	}

	protected void createContent(
		final HasMenuItems parent,
		final Collection<NavigationCategory> categories,
		final Predicate<NavigationItem> itemFilter,
		final List<NavigationItem> items,
		final NavigationHierarchyProvider hierarchyProvider,
		final NavigationHierarchyLevelSorter hierarchyLevelSorter,
		final MenuItemFactory menuItemFactory)
	{
		Stream.concat(
			categories != null ? categories.stream() : Stream.empty(),
			items.stream().filter(itemFilter))
			.sorted(hierarchyLevelSorter)
			.forEach(element -> createMenuElement(element, parent, items, hierarchyProvider,
				hierarchyLevelSorter, menuItemFactory));
	}

	protected void createMenuElement(
		final NavigationElement element,
		final HasMenuItems parent,
		final List<NavigationItem> items,
		final NavigationHierarchyProvider hierarchyProvider,
		final NavigationHierarchyLevelSorter hierarchyLevelSorter,
		final MenuItemFactory menuItemFactory)
	{
		if(element instanceof NavigationCategory)
		{
			final NavigationCategory category = (NavigationCategory)element;

			MenuItem categoryItem = null;
			if(parent instanceof MenuBar)
			{
				categoryItem = ((MenuBar)parent).addItem(category.displayName());
			}
			else if(parent instanceof SubMenu)
			{
				categoryItem = ((SubMenu)parent).addItem(category.displayName());
			}
			final SubMenu categoryMenu = categoryItem.getSubMenu();

			createContent(categoryMenu, hierarchyProvider.getChildCategories(category),
				item -> category.displayName().equals(item.category()), items, hierarchyProvider,
				hierarchyLevelSorter, menuItemFactory);
		}
		else
		{
			final NavigationItem item = (NavigationItem)element;
			menuItemFactory.apply(item, parent);
		}
	}
}
