
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasDataOpacity extends Chart
{
	public default Double getDataOpacity()
	{
		return properties().get("dataOpacity");
	}
	
	public default void setDataOpacity(final Double dataOpacity)
	{
		properties().put("dataOpacity", dataOpacity);
	}
}
