
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasTitle extends Chart
{
	public default String getTitle()
	{
		return properties().get("title");
	}

	public default void setTitle(final String title)
	{
		properties().put("title", title);
	}
	
	public default TextStyle getTitleTextStyle()
	{
		return properties().get("titleTextStyle");
	}
	
	public default void setTitleTextStyle(final TextStyle titleTextStyle)
	{
		properties().put("titleTextStyle", titleTextStyle);
	}
}
