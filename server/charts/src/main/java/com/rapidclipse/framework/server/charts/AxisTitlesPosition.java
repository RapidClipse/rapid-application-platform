package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;

public enum AxisTitlesPosition implements JavaScriptable
{
	IN("in"),
	OUT("out"),
	NONE("none");

	private final String js;

	private AxisTitlesPosition(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}