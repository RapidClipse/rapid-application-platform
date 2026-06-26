/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.maps;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum RegionInteractivity implements JavaScriptable
{
	AUTO(null),
	TRUE("true"),
	FALSE("false");
	
	private final String js;
	
	private RegionInteractivity(final String js)
	{
		this.js = js;
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
