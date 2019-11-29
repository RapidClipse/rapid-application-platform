
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasLegend extends Chart
{
	public default Legend getLegend()
	{
		return properties().get("legend");
	}
	
	public default void setLegend(final Legend legend)
	{
		properties().put("legend", legend);
	}
}
