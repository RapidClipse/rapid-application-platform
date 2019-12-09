/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
