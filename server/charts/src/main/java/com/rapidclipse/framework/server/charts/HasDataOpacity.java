
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasDataOpacity extends Chart
{
	public default Number getDataOpacity()
	{
		return properties().get("dataOpacity");
	}

	public default void setDataOpacity(final Number dataOpacity)
	{
		properties().put("dataOpacity", dataOpacity);
	}
}
