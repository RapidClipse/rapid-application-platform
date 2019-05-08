
package com.rapidclipse.framework.server.data.provider;

import com.rapidclipse.framework.server.data.filter.Filter;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;


/**
 * @author XDEV Software
 *
 */
public interface DataProviderFilterAdapter<D extends ConfigurableFilterDataProvider<?, ?, ?>>
{
	public boolean supports(ConfigurableFilterDataProvider<?, ?, ?> dataProvider);

	public void updateFilter(D dataProvider, Filter filter);
}
