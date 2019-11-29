
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasSizeAxis extends Chart
{
	public default SizeAxis getSizeAxis()
	{
		return properties().get("sizeAxis");
	}

	public default void setSizeAxis(final SizeAxis sizeAxis)
	{
		properties().put("sizeAxis", sizeAxis);
	}
}
