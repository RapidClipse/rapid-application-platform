
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasSeries<S extends Series> extends Chart
{
	public default void addSeries(final int index, final S series)
	{
		properties().putIndexed("series", index, series);
	}

	public default S removeSeries(final int index)
	{
		return properties().removeIndexed("series", index);
	}

	public default void removeAllSeries()
	{
		properties().removeAllIndexed("series");
	}
}
