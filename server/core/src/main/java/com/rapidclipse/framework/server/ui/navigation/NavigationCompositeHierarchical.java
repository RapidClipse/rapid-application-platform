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
import java.util.function.Consumer;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
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
		final List<NavigationItem>               items                    = getItemProvider().getItems();
		final NavigationCategoryComponentFactory categoryComponentFactory = getCategoryComponentFactory();
		final NavigationItemComponentFactory     itemComponentFactory     = getItemComponentFactory();
		
		for(final NavigationCategory category : getHierarchyProvider().getRootCategories())
		{
			createCategoryContent(category, content::add, hierarchyProvider, items, categoryComponentFactory,
				itemComponentFactory);
		}
	}
	
	protected void createCategoryContent(
		final NavigationCategory category,
		final Consumer<Component[]> contentAdder,
		final NavigationHierarchyProvider hierarchyProvider,
		final List<NavigationItem> items,
		final NavigationCategoryComponentFactory categoryComponentFactory,
		final NavigationItemComponentFactory itemComponentFactory)
	{
		final NavigationCategoryComponent categoryComponent = categoryComponentFactory.apply(category);
		contentAdder.accept(new Component[]{categoryComponent.component()});
		
		final Iterable<NavigationCategory> childCategories = hierarchyProvider.getChildCategories(category);
		if(childCategories != null)
		{
			for(final NavigationCategory childCategory : childCategories)
			{
				createCategoryContent(childCategory, categoryComponent.contentAdder(), hierarchyProvider, items,
					categoryComponentFactory,
					itemComponentFactory);
			}
		}
		
		final Component[] itemComponents = items.stream()
			.filter(item -> category.displayName().equals(item.category()))
			.map(itemComponentFactory)
			.toArray(Component[]::new);
		categoryComponent.contentAdder().accept(itemComponents);
	}
}
