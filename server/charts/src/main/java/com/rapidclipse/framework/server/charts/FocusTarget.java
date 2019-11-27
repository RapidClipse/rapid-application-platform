package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;

public enum FocusTarget implements JavaScriptable
{
	DATUM("datum"),
	CATEGORY("category");

	private final String js;

	private FocusTarget(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}