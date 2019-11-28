
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasLineWidth extends Chart
{
	public default Double getLineWidth()
	{
		return properties().get("lineWidth");
	}

	public default void setLineWidth(final Double lineWidth)
	{
		properties().put("lineWidth", lineWidth);
	}
}
