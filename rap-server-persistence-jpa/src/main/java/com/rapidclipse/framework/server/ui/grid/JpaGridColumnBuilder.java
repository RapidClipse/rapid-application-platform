/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.grid;

import static java.util.Objects.requireNonNull;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.jpa.AttributeChain;
import com.rapidclipse.framework.server.jpa.Jpa;
import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.function.ValueProvider;


/**
 * @author XDEV Software
 *
 */
public interface JpaGridColumnBuilder<T>
{
	public JpaGridColumnBuilder<T> withValueProvider(ValueProvider<T, ?> valueProvider);
	
	public JpaGridColumnBuilder<T> withCaption(String caption);
	
	public Column<T> addTo(Grid<T> grid);

	public static <T> JpaGridColumnBuilder<T> For(final Attribute<T, ?> attribute)
	{
		return For(AttributeChain.New(attribute));
	}

	public static <T> JpaGridColumnBuilder<T> For(final AttributeChain<T, ?> attributeChain)
	{
		return new AttributeBased<>(attributeChain.clone());
	}
	
	public static class AttributeBased<T> implements JpaGridColumnBuilder<T>
	{
		private final AttributeChain<T, ?> attributeChain;
		private ValueProvider<T, ?>        valueProvider;
		private String                     caption;
		
		protected AttributeBased(final AttributeChain<T, ?> attributeChain)
		{
			super();
			this.attributeChain = requireNonNull(attributeChain);
		}
		
		@Override
		public JpaGridColumnBuilder<T> withValueProvider(final ValueProvider<T, ?> valueProvider)
		{
			this.valueProvider = valueProvider;
			return this;
		}
		
		@Override
		public JpaGridColumnBuilder<T> withCaption(final String caption)
		{
			this.caption = caption;
			return this;
		}
		
		@Override
		public Column<T> addTo(final Grid<T> grid)
		{
			final AttributeChain<T, ?> attributeChain = this.attributeChain;
			final Attribute<?, ?>      attribute      = attributeChain.last();
			final String               key            = Jpa.toPropertyPath(attributeChain);
			final boolean              sortable       = Comparable.class.isAssignableFrom(attribute.getJavaType());
			ValueProvider<T, ?>        valueProvider  = this.valueProvider;
			String                     caption        = this.caption;
			
			if(valueProvider == null)
			{
				valueProvider = entity -> Jpa.resolveValue(entity, attributeChain);
			}
			
			if(caption == null)
			{
				caption = CaptionUtils.resolveCaption(
					attribute.getDeclaringType().getJavaType(), attribute.getName());
			}

			return grid
				.addColumn(valueProvider)
				.setKey(key)
				.setHeader(caption)
				.setSortable(sortable);
		}
	}
}
