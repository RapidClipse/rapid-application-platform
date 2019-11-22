
package com.rapidclipse.framework.server.charts;

import java.util.EventObject;


/**
 * @author XDEV Software
 *
 */
public class ChartConfigurationChangeEvent<C extends ChartConfiguration<C>> extends EventObject
{
	public ChartConfigurationChangeEvent(final C configuration)
	{
		super(configuration);
	}

	@SuppressWarnings("unchecked") // type safety ensured by constructor
	@Override
	public C getSource()
	{
		return (C)super.getSource();
	}
}
