/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.provider;

import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.data.filter.FilterConverter;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.ListDataProvider;


public class ListDataProviderFilterAdapter
	implements DataProviderFilterAdapter<ListDataProvider<?>>
{
	public ListDataProviderFilterAdapter()
	{
		super();
	}

	@Override
	public boolean supports(final ConfigurableFilterDataProvider<?, ?, ?> dataProvider)
	{
		return dataProvider instanceof ListDataProvider<?>;
	}

	@Override
	public void updateFilter(final ListDataProvider<?> dataProvider, final Filter filter)
	{
		dataProvider.setFilter(FilterConverter.toSerializablePredicate(filter));
	}
}
