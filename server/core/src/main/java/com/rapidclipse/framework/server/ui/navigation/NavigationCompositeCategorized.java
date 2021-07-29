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
