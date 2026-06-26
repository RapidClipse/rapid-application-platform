/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.annotation;

import com.rapidclipse.framework.server.charts.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum ScaleType implements JavaScriptable
{
	FIXED("fixed"),
	MAXIMIZED("maximized"),
	ALL_FIXED("allfixed"),
	ALL_MAXIMIZED("allmaximized");
	
	private final String js;
	
	private ScaleType(final String js)
	{
		this.js = Json.create(js).toJson();
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
