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
		if(this.hierarchyProvider == null)
		{
			this.hierarchyProvider = NavigationHierarchyProvider.ForItems(getItemProvider().getItems());
		}
		
		return this.hierarchyProvider;
	}
	
	public void setMenuItemFactory(final MenuItemFactory menuItemFactory)
	{
		this.menuItemFactory = requireNonNull(menuItemFactory);
	}
	
	public MenuItemFactory getMenuItemFactory()
	{
		if(this.menuItemFactory == null)
		{
			this.menuItemFactory = MenuItemFactory.Default();
		}
		
		return this.menuItemFactory;
	}
	
	public void setHierarchyLevelSorter(final NavigationHierarchyLevelSorter hierarchyLevelSorter)
	{
		this.hierarchyLevelSorter = requireNonNull(hierarchyLevelSorter);
	}
	
	public NavigationHierarchyLevelSorter getHierarchyLevelSorter()
	{
		if(this.hierarchyLevelSorter == null)
		{
			this.hierarchyLevelSorter = NavigationHierarchyLevelSorter.Default();
		}
		
		return this.hierarchyLevelSorter;
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