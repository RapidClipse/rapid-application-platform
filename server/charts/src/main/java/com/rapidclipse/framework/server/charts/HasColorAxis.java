
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
