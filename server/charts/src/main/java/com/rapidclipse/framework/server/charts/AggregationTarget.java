package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;

public enum AggregationTarget implements JavaScriptable
{
	CATEGORY("category"),
	SERIES("series"),
	AUTO("auto"),
	NONE("none");

	private final String js;

	private AggregationTarget(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}