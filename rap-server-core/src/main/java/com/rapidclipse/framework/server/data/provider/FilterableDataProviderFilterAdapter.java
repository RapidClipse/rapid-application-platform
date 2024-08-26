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
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;


public class FilterableDataProviderFilterAdapter
	implements DataProviderFilterAdapter<FilterableDataProvider<?, ?>>
{
	public FilterableDataProviderFilterAdapter()
	{
		super();
	}
	
	@Override
	public boolean supports(final ConfigurableFilterDataProvider<?, ?, ?> dataProvider)
	{
		return dataProvider instanceof FilterableDataProvider<?, ?>;
	}

	@Override
	public void updateFilter(final FilterableDataProvider<?, ?> dataProvider, final Filter filter)
	{
		dataProvider.setFilter(filter);
	}
}
