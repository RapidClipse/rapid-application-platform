
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasLineColor extends HasLineWidth
{
	public default String getLineColor()
	{
		return properties().get("lineColor");
	}
	
	public default void setLineColor(final String lineColor)
	{
		properties().put("lineColor", lineColor);
	}
}
