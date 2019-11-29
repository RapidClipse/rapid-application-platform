
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
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
