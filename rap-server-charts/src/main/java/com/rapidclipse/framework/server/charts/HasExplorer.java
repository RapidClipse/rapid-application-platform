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
public interface HasExplorer extends Chart
{
	public default Explorer getExplorer()
	{
		return properties().get("explorer", null);
	}
	
	public default void setExplorer(final Explorer explorer)
	{
		properties().put("explorer", explorer);
	}
}
