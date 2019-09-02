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
