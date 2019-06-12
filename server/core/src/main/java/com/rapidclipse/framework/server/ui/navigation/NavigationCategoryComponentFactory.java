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
			details.setContent(content);

			return NavigationCategoryComponent.New(details, content::add);
		}
	}
}
