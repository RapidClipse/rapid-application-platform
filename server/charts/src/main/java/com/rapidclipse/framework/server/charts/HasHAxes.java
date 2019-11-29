
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasHAxes extends HasHAxis
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
