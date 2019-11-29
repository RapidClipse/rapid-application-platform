
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasValueRange extends Chart
{
	public default Number getMin()
	{
		return properties().get("min");
	}
	
	public default void setMin(final Number min)
	{
		properties().put("min", min);
	}
	
	public default Number getMax()
	{
		return properties().get("max");
	}
	
	public default void setMax(final Number max)
	{
		properties().put("max", max);
	}
}
