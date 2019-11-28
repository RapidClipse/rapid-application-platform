
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasVAxes extends Chart
{
	public default void addVAxis(final int index, final Axis axis)
	{
		properties().putIndexed("vAxes", index, axis);
	}
	
	public default Axis removeVAxis(final int index)
	{
		return properties().removeIndexed("vAxes", index);
	}
	
	public default void removeAllVAxes()
	{
		properties().removeAllIndexed("vAxes");
	}
}
