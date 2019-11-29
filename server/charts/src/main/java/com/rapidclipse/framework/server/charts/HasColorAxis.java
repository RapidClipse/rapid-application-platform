
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasColorAxis extends Chart
{
	public default ColorAxis getColorAxis()
	{
		return properties().get("colorAxis");
	}
	
	public default void setColorAxis(final ColorAxis colorAxis)
	{
		properties().put("colorAxis", colorAxis);
	}
}
