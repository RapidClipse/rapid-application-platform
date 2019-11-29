
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasOrientation extends Chart
{
	public default Orientation getOrientation()
	{
		return properties().get("orientation");
	}

	public default void setOrientation(final Orientation orientation)
	{
		properties().put("orientation", orientation);
	}
}
