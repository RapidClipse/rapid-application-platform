/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.navigation;

import static java.util.Objects.requireNonNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.rapidclipse.framework.server.navigation.NavigationElement;
import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;


/**
 * @author XDEV Software
 *
 */
public abstract class NavigationCompositeHierarchical<T extends Component & HasComponents>
	extends NavigationComposite<T>
{
	private NavigationHierarchyProvider        hierarchyProvider;
	private NavigationHierarchyLevelSorter     hierarchyLevelSorter;
	private NavigationCategoryComponentFactory categoryComponentFactory;
	private NavigationItemComponentFactory     itemComponentFactory;

	protected NavigationCompositeHierarchical()
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

	public void setHierarchyLevelSorter(final NavigationHierarchyLevelSorter hierarchyLevelSorter)
	{
		this.hierarchyLevelSorter = hierarchyLevelSorter;
	}

	public NavigationHierarchyLevelSorter getHierarchyLevelSorter()
	{
		return this.hierarchyLevelSorter != null
			? this.hierarchyLevelSorter
			: (this.hierarchyLevelSorter = NavigationHierarchyLevelSorter.Default());
	}

	public void setCategoryComponentFactory(final NavigationCategoryComponentFactory categoryComponentFactory)
	{
		this.categoryComponentFactory = requireNonNull(categoryComponentFactory);
	}

	public NavigationCategoryComponentFactory getCategoryComponentFactory()
	{
		return this.categoryComponentFactory != null
			? this.categoryComponentFactory
			: (this.categoryComponentFactory = NavigationCategoryComponentFactory.DetailsFactory());
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

		final NavigationHierarchyProvider        hierarchyProvider        = getHierarchyProvider();
		final NavigationHierarchyLevelSorter     hierarchyLevelSorter     = getHierarchyLevelSorter();
		final List<NavigationItem>               items                    = getItemProvider().getItems();
		final NavigationCategoryComponentFactory categoryComponentFactory = getCategoryComponentFactory();
		final NavigationItemComponentFactory     itemComponentFactory     = getItemComponentFactory();

		createContent(c -> content.add(c), hierarchyProvider.getRootCategories(),
			item -> StringUtils.isEmpty(item.category()), items, hierarchyProvider, hierarchyLevelSorter,
			categoryComponentFactory, itemComponentFactory);
	}

	protected void createContent(
		final Consumer<Component[]> contentAdder,
		final Collection<NavigationCategory> categories,
		final Predicate<NavigationItem> itemFilter,
		final List<NavigationItem> items,
		final NavigationHierarchyProvider hierarchyProvider,
		final NavigationHierarchyLevelSorter hierarchyLevelSorter,
		final NavigationCategoryComponentFactory categoryComponentFactory,
		final NavigationItemComponentFactory itemComponentFactory)
	{
		final Component[] components = Stream.concat(
			categories != null ? categories.stream() : Stream.empty(),
			items.stream().filter(itemFilter))
			.sorted(hierarchyLevelSorter)
			.map(element -> createComponent(element, items, hierarchyProvider, hierarchyLevelSorter,
				categoryComponentFactory, itemComponentFactory))
			.toArray(Component[]::new);
		contentAdder.accept(components);
	}

	protected Component createComponent(
		final NavigationElement element,
		final List<NavigationItem> items,
		final NavigationHierarchyProvider hierarchyProvider,
		final NavigationHierarchyLevelSorter hierarchyLevelSorter,
		final NavigationCategoryComponentFactory categoryComponentFactory,
		final NavigationItemComponentFactory itemComponentFactory)
	{
		if(element instanceof NavigationCategory)
		{
			final NavigationCategory category = (NavigationCategory)element;

			final NavigationCategoryComponent categoryComponent = categoryComponentFactory.apply(category);

			createContent(categoryComponent.contentAdder(), hierarchyProvider.getChildCategories(category),
				item -> category.displayName().equals(item.category()), items, hierarchyProvider,
				hierarchyLevelSorter, categoryComponentFactory, itemComponentFactory);

			return categoryComponent.component();
		}

		final NavigationItem item = (NavigationItem)element;
		return itemComponentFactory.apply(item);
	}
}
