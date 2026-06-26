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
public interface HasAreaOpacity extends Chart
{
	public default double getAreaOpacity()
	{
		return properties().get("areaOpacity", 0.3);
	}

	public default void setAreaOpacity(final double areaOpacity)
	{
		properties().put("areaOpacity", areaOpacity);
	}
}
