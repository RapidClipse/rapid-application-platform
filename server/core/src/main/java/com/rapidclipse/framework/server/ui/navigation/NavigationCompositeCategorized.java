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
