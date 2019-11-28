
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasVAxis extends Chart
{
	public default Axis getVAxis()
	{
		return properties().get("vAxis");
	}

	public default void setVAxis(final Axis vAxis)
	{
		properties().put("vAxis", vAxis);
	}
}
