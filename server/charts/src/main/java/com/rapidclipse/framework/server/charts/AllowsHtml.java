
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface AllowsHtml extends Chart
{
	public default Boolean getAllowHtml()
	{
		return properties().get("allowHtml");
	}
	
	public default void setAllowHtml(final Boolean allowHtml)
	{
		properties().put("allowHtml", allowHtml);
	}
}
