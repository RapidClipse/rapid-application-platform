
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasTheme extends Chart
{
	public default Theme getTheme()
	{
		return properties().get("theme");
	}
	
	public default void setTheme(final Theme theme)
	{
		properties().put("theme", theme);
	}
}
