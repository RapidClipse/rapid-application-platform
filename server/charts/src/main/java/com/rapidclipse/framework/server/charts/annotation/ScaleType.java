
package com.rapidclipse.framework.server.charts.annotation;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


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
