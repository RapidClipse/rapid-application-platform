
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
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
