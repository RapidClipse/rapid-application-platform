
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasTitlePosition extends HasTitle
{
	public default TextPosition getTitlePosition()
	{
		return properties().get("titlePosition");
	}

	public default void setTitlePosition(final TextPosition titlePosition)
	{
		properties().put("titlePosition", titlePosition);
	}
}
