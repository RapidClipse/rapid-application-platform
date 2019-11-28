
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasBar extends Chart
{
	public default Bar getBar()
	{
		return properties().get("bar");
	}
	
	public default void setBar(final Bar bar)
	{
		properties().put("bar", bar);
	}
}
