
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasTooltip extends Chart
{
	public default Tooltip getTooltip()
	{
		return properties().get("tooltip");
	}

	public default void setTooltip(final Tooltip tooltip)
	{
		properties().put("tooltip", tooltip);
	}
}
