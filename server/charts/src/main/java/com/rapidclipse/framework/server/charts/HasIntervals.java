
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasIntervals extends Chart
{
	public default void addInterval(final String column, final Interval interval)
	{
		properties().putIndexed("interval", column, interval);
	}
	
	public default Interval removeInterval(final String column)
	{
		return properties().removeIndexed("interval", column);
	}
	
	public default void removeAllIntervals()
	{
		properties().removeAllIndexed("interval");
	}
	
	public default void setInterval(final Interval interval)
	{
		properties().put("intervals", interval);
	}

	public default Interval getInterval()
	{
		return properties().get("intervals");
	}
}
