
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasChartSize extends Chart
{
	public default Number getChartWidth()
	{
		return properties().get("width");
	}
	
	public default void setChartWidth(final Number width)
	{
		properties().put("width", width);
	}

	public default Number getChartHeight()
	{
		return properties().get("height");
	}
	
	public default void setChartHeight(final Number height)
	{
		properties().put("height", height);
	}
}
