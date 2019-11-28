
package com.rapidclipse.framework.server.charts.annotation;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


public enum LegendPosition implements JavaScriptable
{
	SAME_ROW("sameRow"),
	NEW_ROW("newRow");
	
	private final String js;
	
	private LegendPosition(final String js)
	{
		this.js = Json.create(js).toJson();
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
