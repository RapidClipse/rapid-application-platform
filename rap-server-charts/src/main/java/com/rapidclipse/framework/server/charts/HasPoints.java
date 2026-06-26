/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasPoints extends Chart
{
	public default PointShape getPointShape()
	{
		return properties().get("pointShape", null);
	}

	public default void setPointShape(final PointShape pointShape)
	{
		properties().put("pointShape", pointShape);
	}
	
	public default Number getPointSize()
	{
		return properties().get("pointSize", 0);
	}
	
	public default void setPointSize(final Number pointSize)
	{
		properties().put("pointSize", pointSize);
	}
	
	public default boolean getPointsVisible()
	{
		return properties().get("pointsVisible", true);
	}
	
	public default void setPointsVisible(final boolean pointsVisible)
	{
		properties().put("pointsVisible", pointsVisible);
	}
}
