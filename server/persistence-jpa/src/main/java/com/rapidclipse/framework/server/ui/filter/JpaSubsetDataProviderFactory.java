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
public final class JpaSubsetDataProviderFactory
{
	public static <T> SubsetDataProvider<T> createCriteriaSubsetDataProvider(
		final Attribute<T, ?> attribute)
	{
		final Class<T>         entityType = attribute.getDeclaringType().getJavaType();
		final CriteriaQuery<T> criteria   = Jpa.createCriteriaQuery(entityType);
		criteria.from(entityType);
		
		return createCriteriaSubsetDataProvider(criteria, attribute);
	}
	
	public static <T> SubsetDataProvider<T> createCriteriaSubsetDataProvider(
		final CriteriaQuery<T> criteria,
		final Attribute<T, ?> attribute)
	{
		return createCriteriaSubsetDataProvider(criteria, attribute.getName());
	}
	
	public static <T> SubsetDataProvider<T> createCriteriaSubsetDataProvider(
		final CriteriaQuery<T> criteria,
		final String attributeName)
	{
		final CriteriaDataProvider<T>              dataProvider       = CriteriaDataProvider.New(criteria);
		final SerializableFunction<String, Filter> filterConverter    = FilterConverterFactory
			.New(attributeName);
		final ItemLabelGenerator<T>                itemLabelGenerator = entity -> {
																			final Object value =
																				Jpa.resolveValue(entity, attributeName);
																			return String.valueOf(value);
																		};
		
		return SubsetDataProvider.New(dataProvider, filterConverter, itemLabelGenerator);
	}
	
	private JpaSubsetDataProviderFactory()
	{
		throw new Error();
	}
}
