
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasFont extends Chart
{
	public default String getFontName()
	{
		return properties().get("fontName");
	}
	
	public default void setFontName(final String fontName)
	{
		properties().put("fontName", fontName);
	}

	public default Double getFontSize()
	{
		return properties().get("fontSize");
	}
	
	public default void setFontSize(final Double fontSize)
	{
		properties().put("fontSize", fontSize);
	}
}
