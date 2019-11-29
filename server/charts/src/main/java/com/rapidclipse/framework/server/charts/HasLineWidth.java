
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasLineWidth extends Chart
{
	public default Number getLineWidth()
	{
		return properties().get("lineWidth");
	}
	
	public default void setLineWidth(final Number lineWidth)
	{
		properties().put("lineWidth", lineWidth);
	}
}
