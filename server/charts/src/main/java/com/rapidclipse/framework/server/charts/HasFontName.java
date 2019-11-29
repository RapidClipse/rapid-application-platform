
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasFontName extends Chart
{
	public default String getFontName()
	{
		return properties().get("fontName");
	}

	public default void setFontName(final String fontName)
	{
		properties().put("fontName", fontName);
	}
}
