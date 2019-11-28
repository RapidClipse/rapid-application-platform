
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasChartArea extends Chart
{
	public default ChartArea getChartArea()
	{
		return properties().get("chartArea");
	}
	
	public default void setChartArea(final ChartArea chartArea)
	{
		properties().put("chartArea", chartArea);
	}
}
