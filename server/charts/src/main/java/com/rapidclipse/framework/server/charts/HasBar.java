
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
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
