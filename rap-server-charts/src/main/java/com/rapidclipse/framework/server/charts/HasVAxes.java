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
public interface HasVAxes extends HasVAxis
{
	public default void addVAxis(final int index, final Axis axis)
	{
		properties().putIndexed("vAxes", index, axis);
	}
	
	public default Axis removeVAxis(final int index)
	{
		return properties().removeIndexed("vAxes", index);
	}
	
	public default void removeAllVAxes()
	{
		properties().removeAllIndexed("vAxes");
	}
}
