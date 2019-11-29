
package com.rapidclipse.framework.server.charts;

import java.util.List;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasColors extends Chart
{
	public default List<String> getColors()
	{
		return properties().get("colors");
	}
	
	public default void setColors(final List<String> colors)
	{
		properties().put("colors", colors);
	}
}
