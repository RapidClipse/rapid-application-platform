
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasVAxes extends HasVAxis
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
