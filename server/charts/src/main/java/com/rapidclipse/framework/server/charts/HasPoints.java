
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasPoints extends Chart
{
	public default PointShape getPointShape()
	{
		return properties().get("pointShape");
	}
	
	public default void setPointShape(final PointShape pointShape)
	{
		properties().put("pointShape", pointShape);
	}

	public default Number getPointSize()
	{
		return properties().get("pointSize");
	}

	public default void setPointSize(final Number pointSize)
	{
		properties().put("pointSize", pointSize);
	}

	public default Boolean getPointsVisible()
	{
		return properties().get("pointsVisible");
	}

	public default void setPointsVisible(final Boolean pointsVisible)
	{
		properties().put("pointsVisible", pointsVisible);
	}
}
