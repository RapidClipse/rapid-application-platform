
package com.rapidclipse.framework.server.charts;

import java.util.List;


/**
 * @author XDEV Software
 *
 */
public interface HasLineDashStyle extends HasLineWidth
{
	public default List<Number> getLineDashStyle()
	{
		return properties().get("lineDashStyle");
	}

	public default void setLineDashStyle(final List<Number> lineDashStyle)
	{
		properties().put("lineDashStyle", lineDashStyle);
	}
}
