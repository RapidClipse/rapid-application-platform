/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.grid;

import static java.util.Objects.requireNonNull;

import jakarta.persistence.metamodel.Attribute;

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
