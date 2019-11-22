package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;

public enum TextPosition implements JavaScriptable
{
	OUT("out"),
	IN("in"),
	NONE("none");
	
	private final String js;
	
	private TextPosition(final String js)
	{
		this.js = Json.create(js).toJson();
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}