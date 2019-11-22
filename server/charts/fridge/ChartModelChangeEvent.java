
package com.rapidclipse.framework.server.charts;

import java.util.EventObject;


/**
 * @author XDEV Software
 *
 */
public class ChartModelChangeEvent extends EventObject
{
	public ChartModelChangeEvent(final ChartModel model)
	{
		super(model);
	}

	@Override
	public ChartModel getSource()
	{
		return (ChartModel)super.getSource();
	}
}
