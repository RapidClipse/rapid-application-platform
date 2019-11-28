
package com.rapidclipse.framework.server.charts.maps;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


public enum Resolution implements JavaScriptable
{
	COUNTRIES("countries"),
	PROVINCES("provinces"),
	METROS("metros");

	private final String js;

	private Resolution(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}