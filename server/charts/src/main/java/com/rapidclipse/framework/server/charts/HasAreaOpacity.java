
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
