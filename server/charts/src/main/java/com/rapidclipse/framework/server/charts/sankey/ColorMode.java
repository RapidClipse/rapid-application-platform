
package com.rapidclipse.framework.server.charts.sankey;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


public enum ColorMode implements JavaScriptable
{
	SOURCE("source"),
	TARGET("target"),
	GRADIENT("gradient"),
	NONE("none");
	
	private final String js;
	
	private ColorMode(final String js)
	{
		this.js = Json.create(js).toJson();
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
