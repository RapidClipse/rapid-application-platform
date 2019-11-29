
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasFont extends HasFontName
{
	public default Double getFontSize()
	{
		return properties().get("fontSize");
	}

	public default void setFontSize(final Double fontSize)
	{
		properties().put("fontSize", fontSize);
	}
}
