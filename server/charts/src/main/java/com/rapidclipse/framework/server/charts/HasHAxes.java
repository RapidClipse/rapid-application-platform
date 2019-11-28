
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasHAxes extends Chart
{
	public default void addHAxis(final int index, final Axis axis)
	{
		properties().putIndexed("hAxes", index, axis);
	}

	public default Axis removeHAxis(final int index)
	{
		return properties().removeIndexed("hAxes", index);
	}

	public default void removeAllHAxes()
	{
		properties().removeAllIndexed("hAxes");
	}
}
