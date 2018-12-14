/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.filter;


import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.metamodel.Attribute;

import com.vaadin.flow.component.ItemLabelGenerator;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.function.SerializableFunction;

import software.xdev.rap.server.data.filter.Filter;
import software.xdev.rap.server.data.provider.JpaDataProviderFactory;
import software.xdev.rap.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public final class JpaSubsetDataProviderFactory
{
	public static <T> SubsetDataProvider<T> createCriteriaSubsetDataProvider(
			final Attribute<T, ?> attribute)
	{
		final Class<T> entityType = attribute.getDeclaringType().getJavaType();
		final CriteriaQuery<T> criteria = Jpa.createCriteriaQuery(entityType);
		criteria.from(entityType);
		
		return createCriteriaSubsetDataProvider(criteria,attribute);
	}
	
	
	public static <T> SubsetDataProvider<T> createCriteriaSubsetDataProvider(
			final CriteriaQuery<T> criteria, final Attribute<T, ?> attribute)
	{
		return createCriteriaSubsetDataProvider(criteria,attribute.getName());
	}
	
	
	public static <T> SubsetDataProvider<T> createCriteriaSubsetDataProvider(
			final CriteriaQuery<T> criteria, final String attributeName)
	{
		final DataProvider<T, Filter> dataProvider = JpaDataProviderFactory
				.createCriteriaDataProvider(criteria);
		final SerializableFunction<String, Filter> filterConverter = FilterConverterFactory
				.New(attributeName);
		final ItemLabelGenerator<T> itemLabelGenerator = entity -> String
				.valueOf(Jpa.resolveValue(entity,attributeName));
		return SubsetDataProvider.New(dataProvider,filterConverter,itemLabelGenerator);
	}
	
	
	private JpaSubsetDataProviderFactory()
	{
		throw new Error();
	}
}
