
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasChartSize extends Chart
{
	public default Double getChartWidth()
	{
		return properties().get("width");
	}

	public default void setChartWidth(final Double width)
	{
		properties().put("width", width);
	}
	
	public default Double getChartHeight()
	{
		return properties().get("height");
	}

	public default void setChartHeight(final Double height)
	{
		properties().put("height", height);
	}
}
