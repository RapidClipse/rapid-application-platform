/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.navigation;

import java.util.function.Supplier;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.details.Details;
import com.vaadin.flow.component.details.DetailsVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationCategoryComponentFactory
	extends SerializableFunction<NavigationCategory, NavigationCategoryComponent>
{
	public static NavigationCategoryComponentFactory DetailsFactory()
	{
		return new DetailsFactory();
	}
	
	public static class DetailsFactory implements NavigationCategoryComponentFactory
	{
		protected DetailsFactory()
		{
			super();
		}

		@Override
		public NavigationCategoryComponent apply(final NavigationCategory category)
		{
			final Details details = new Details();
			details.addThemeVariants(DetailsVariant.SMALL);
			
			final Supplier<Component> icon = category.icon();
			if(icon != null)
			{
				details.setSummary(new HorizontalLayout(
					icon.get(),
					new Span(category.displayName())));
			}
			else
			{
				details.setSummaryText(category.displayName());
			}

			final VerticalLayout content = new VerticalLayout();
			content.setSpacing(false);
			content.setMargin(false);
			content.setPadding(false);
			details.removeAll();
			details.add(content);

			return NavigationCategoryComponent.New(details, content::add);
		}
	}
}
