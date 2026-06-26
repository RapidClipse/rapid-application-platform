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
public interface HasSeries<S extends Series> extends Chart
{
	public default void addSeries(final int index, final S series)
	{
		properties().putIndexed("series", index, series);
	}
	
	public default S removeSeries(final int index)
	{
		return properties().removeIndexed("series", index);
	}
	
	public default void removeAllSeries()
	{
		properties().removeAllIndexed("series");
	}
}
