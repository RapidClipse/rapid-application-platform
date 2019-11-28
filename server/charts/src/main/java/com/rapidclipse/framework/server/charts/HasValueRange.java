
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasValueRange extends Chart
{
	public default Double getMin()
	{
		return properties().get("min");
	}

	public default void setMin(final Double min)
	{
		properties().put("min", min);
	}

	public default Double getMax()
	{
		return properties().get("max");
	}

	public default void setMax(final Double max)
	{
		properties().put("max", max);
	}
}
