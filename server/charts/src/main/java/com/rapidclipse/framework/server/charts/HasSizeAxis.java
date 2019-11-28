
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
