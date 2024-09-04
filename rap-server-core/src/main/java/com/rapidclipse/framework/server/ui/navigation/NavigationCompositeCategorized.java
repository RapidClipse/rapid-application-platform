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

import java.util.List;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;


/**
 * @author XDEV Software
 *
 */
public abstract class NavigationCompositeCategorized<T extends Component & HasComponents>
	extends NavigationComposite<T>
{
	private NavigationCategoriesProvider       categoriesProvider;
	private NavigationCategoryComponentFactory categoryComponentFactory;
	private NavigationItemComponentFactory     itemComponentFactory;
	
	protected NavigationCompositeCategorized()
	{
		super();
	}

	public void setCategoriesProvider(final NavigationCategoriesProvider categoriesProvider)
	{
		this.categoriesProvider = requireNonNull(categoriesProvider);
	}

	public NavigationCategoriesProvider getCategoriesProvider()
	{
		return this.categoriesProvider != null
			? this.categoriesProvider
			: (this.categoriesProvider = NavigationCategoriesProvider.ForItems(getItemProvider().getItems()));
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
		
		final List<NavigationItem>               items                    = getItemProvider().getItems();
		final NavigationCategoryComponentFactory categoryComponentFactory = getCategoryComponentFactory();
		final NavigationItemComponentFactory     itemComponentFactory     = getItemComponentFactory();
		
		for(final NavigationCategory category : getCategoriesProvider().getRootCategories())
		{
			final NavigationCategoryComponent categoryComponent = categoryComponentFactory.apply(category);
			content.add(categoryComponent.component());

			final Component[] itemComponents = items.stream()
				.filter(item -> category.displayName().equals(item.category()))
				.map(itemComponentFactory)
				.toArray(Component[]::new);
			categoryComponent.contentAdder().accept(itemComponents);
		}
	}
}
