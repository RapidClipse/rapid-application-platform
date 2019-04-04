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
package com.rapidclipse.framework.server.ui.grid;

import static java.util.Objects.requireNonNull;

import java.util.Collection;

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
public interface JpaGridColumnBuilder
{
	public JpaGridColumnBuilder withValueProvider(ValueProvider<?, ?> valueProvider);

	public JpaGridColumnBuilder withCaption(String caption);

	public <T> Column<T> addTo(Grid<T> grid);

	public static JpaGridColumnBuilder For(final Attribute<?, ?>... attributeChain)
	{
		return new AttributeBased(AttributeChain.New(attributeChain));
	}

	public static JpaGridColumnBuilder For(
		final Collection<? extends Attribute<?, ?>> attributeChain)
	{
		return new AttributeBased(AttributeChain.New(attributeChain));
	}

	public static JpaGridColumnBuilder For(final AttributeChain attributeChain)
	{
		return new AttributeBased(attributeChain.clone());
	}

	public static class AttributeBased implements JpaGridColumnBuilder
	{
		private final AttributeChain attributeChain;
		private ValueProvider<?, ?>  valueProvider;
		private String               caption;

		public AttributeBased(final AttributeChain attributeChain)
		{
			super();
			this.attributeChain = requireNonNull(attributeChain);
		}

		@Override
		public JpaGridColumnBuilder withValueProvider(final ValueProvider<?, ?> valueProvider)
		{
			this.valueProvider = valueProvider;
			return this;
		}

		@Override
		public JpaGridColumnBuilder withCaption(final String caption)
		{
			this.caption = caption;
			return this;
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> Column<T> addTo(final Grid<T> grid)
		{
			final AttributeChain attributeChain = this.attributeChain;
			ValueProvider<T, ?>  valueProvider  = (ValueProvider<T, ?>)this.valueProvider;
			String               caption        = this.caption;
			final String         key            = Jpa.toPropertyPath(attributeChain);

			if(valueProvider == null)
			{
				valueProvider = entity -> Jpa.resolveValue(entity, attributeChain);
			}

			if(caption == null)
			{
				final Attribute<?, ?> attribute = attributeChain.last();
				caption = CaptionUtils.resolveCaption(
					attribute.getDeclaringType().getJavaType(), attribute.getName());
			}

			return grid
				.addColumn(valueProvider)
				.setHeader(caption)
				.setKey(key)
				.setSortable(true);
		}
	}
}
