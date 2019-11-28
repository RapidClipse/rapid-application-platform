
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasBackground extends Chart
{
	public default Background getBackground()
	{
		return properties().get("backgroundColor");
	}

	public default void setBackground(final Background background)
	{
		properties().put("backgroundColor", background);
	}
}
