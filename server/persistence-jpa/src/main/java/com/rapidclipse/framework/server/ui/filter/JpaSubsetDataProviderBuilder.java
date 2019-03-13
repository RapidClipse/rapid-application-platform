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

package com.rapidclipse.framework.server.ui.filter;

import static java.util.Objects.requireNonNull;

import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.data.provider.CriteriaDataProvider;
import com.rapidclipse.framework.server.jpa.Jpa;
import com.vaadin.flow.component.ItemLabelGenerator;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 *
 */
public interface JpaSubsetDataProviderBuilder<T>
{
	public JpaSubsetDataProviderBuilder<T> withDataProvider(CriteriaDataProvider<T> dataProvider);

	public JpaSubsetDataProviderBuilder<T> withFilterConverter(SerializableFunction<String, Filter> filterConverter);

	public JpaSubsetDataProviderBuilder<T> withItemLabelGenerator(ItemLabelGenerator<T> itemLabelGenerator);

	public SubsetDataProvider<T> build();

	public static <T> JpaSubsetDataProviderBuilder<T> For(final Attribute<T, ?> attribute)
	{
		return new AttributeBased<>(attribute);
	}

	public static class AttributeBased<T> implements JpaSubsetDataProviderBuilder<T>
	{
		private final Attribute<T, ?>                attribute;
		private CriteriaDataProvider<T>              dataProvider;
		private SerializableFunction<String, Filter> filterConverter;
		private ItemLabelGenerator<T>                itemLabelGenerator;

		public AttributeBased(final Attribute<T, ?> attribute)
		{
			super();
			this.attribute = requireNonNull(attribute);
		}

		@Override
		public JpaSubsetDataProviderBuilder<T> withDataProvider(final CriteriaDataProvider<T> dataProvider)
		{
			this.dataProvider = dataProvider;
			return this;
		}

		@Override
		public JpaSubsetDataProviderBuilder<T>
			withFilterConverter(final SerializableFunction<String, Filter> filterConverter)
		{
			this.filterConverter = filterConverter;
			return this;
		}

		@Override
		public JpaSubsetDataProviderBuilder<T> withItemLabelGenerator(final ItemLabelGenerator<T> itemLabelGenerator)
		{
			this.itemLabelGenerator = itemLabelGenerator;
			return this;
		}

		@Override
		public SubsetDataProvider<T> build()
		{
			final Attribute<T, ?>                attribute          = this.attribute;
			CriteriaDataProvider<T>              dataProvider       = this.dataProvider;
			SerializableFunction<String, Filter> filterConverter    = this.filterConverter;
			ItemLabelGenerator<T>                itemLabelGenerator = this.itemLabelGenerator;

			if(dataProvider == null)
			{
				final Class<T>         entityType = attribute.getDeclaringType().getJavaType();
				final CriteriaQuery<T> criteria   =
					Jpa.getEntityManager(entityType).getCriteriaBuilder().createQuery(entityType);
				criteria.from(entityType);
				dataProvider = CriteriaDataProvider.New(criteria);
			}

			if(filterConverter == null)
			{
				filterConverter = FilterConverterFactory.createStringConverter(attribute.getName());
			}

			if(itemLabelGenerator == null)
			{
				itemLabelGenerator =
					entity -> String.valueOf(Jpa.resolveValue(entity, attribute));
			}

			return SubsetDataProvider.New(dataProvider, filterConverter, itemLabelGenerator);
		}
	}
}
