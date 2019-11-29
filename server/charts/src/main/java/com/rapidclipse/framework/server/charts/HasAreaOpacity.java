
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasAreaOpacity extends Chart
{
	public default Number getAreaOpacity()
	{
		return properties().get("areaOpacity");
	}

	public default void setAreaOpacity(final Number areaOpacity)
	{
		properties().put("areaOpacity", areaOpacity);
	}
}
