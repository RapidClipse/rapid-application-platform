
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasAreaOpacity extends Chart
{
	public default Double getAreaOpacity()
	{
		return properties().get("areaOpacity");
	}

	public default void setAreaOpacity(final Double areaOpacity)
	{
		properties().put("areaOpacity", areaOpacity);
	}
}
