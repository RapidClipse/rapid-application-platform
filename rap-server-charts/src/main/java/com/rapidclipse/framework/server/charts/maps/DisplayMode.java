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

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum DisplayMode implements JavaScriptable
{
	AUTO("auto"),
	REGIONS("regions"),
	MARKERS("markers"),
	TEXT("text");

	private final String js;

	private DisplayMode(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
