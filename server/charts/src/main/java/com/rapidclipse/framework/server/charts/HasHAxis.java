
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasHAxis extends Chart
{
	public default Axis getHAxis()
	{
		return properties().get("hAxis");
	}

	public default void setHAxis(final Axis hAxis)
	{
		properties().put("hAxis", hAxis);
	}
}
