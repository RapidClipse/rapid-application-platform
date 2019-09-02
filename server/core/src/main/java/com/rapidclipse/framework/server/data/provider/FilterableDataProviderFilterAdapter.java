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
