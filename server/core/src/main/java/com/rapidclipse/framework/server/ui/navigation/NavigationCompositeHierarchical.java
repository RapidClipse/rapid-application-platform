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
		if(this.hierarchyProvider == null)
		{
			this.hierarchyProvider = NavigationHierarchyProvider.ForItems(getItemProvider().getItems());
		}

		return this.hierarchyProvider;
	}

	public void setHierarchyLevelSorter(final NavigationHierarchyLevelSorter hierarchyLevelSorter)
	{
		this.hierarchyLevelSorter = hierarchyLevelSorter;
	}

	public NavigationHierarchyLevelSorter getHierarchyLevelSorter()
	{
		if(this.hierarchyLevelSorter == null)
		{
			this.hierarchyLevelSorter = NavigationHierarchyLevelSorter.Default();
		}

		return this.hierarchyLevelSorter;
	}

	public void setCategoryComponentFactory(final NavigationCategoryComponentFactory categoryComponentFactory)
	{
		this.categoryComponentFactory = requireNonNull(categoryComponentFactory);
	}

	public NavigationCategoryComponentFactory getCategoryComponentFactory()
	{
		if(this.categoryComponentFactory == null)
		{
			this.categoryComponentFactory = NavigationCategoryComponentFactory.DetailsFactory();
		}

		return this.categoryComponentFactory;
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
