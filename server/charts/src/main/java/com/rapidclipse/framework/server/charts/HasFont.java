
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasFont extends HasFontName
{
	public default Number getFontSize()
	{
		return properties().get("fontSize");
	}

	public default void setFontSize(final Number fontSize)
	{
		properties().put("fontSize", fontSize);
	}
}
