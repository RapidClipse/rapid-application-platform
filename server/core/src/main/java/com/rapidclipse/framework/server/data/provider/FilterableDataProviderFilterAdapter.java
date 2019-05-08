
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
