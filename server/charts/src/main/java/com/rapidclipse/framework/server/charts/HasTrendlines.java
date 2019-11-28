
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasTrendlines extends Chart
{
	public default void addTrendline(final int index, final Trendline trendline)
	{
		properties().putIndexed("trendlines", index, trendline);
	}

	public default Trendline removeTrendline(final int index)
	{
		return properties().removeIndexed("trendlines", index);
	}

	public default void removeAllTrendlines()
	{
		properties().removeAllIndexed("trendlines");
	}
}
