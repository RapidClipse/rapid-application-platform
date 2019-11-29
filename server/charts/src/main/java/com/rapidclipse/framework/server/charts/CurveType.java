
package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum CurveType implements JavaScriptable
{
	NONE("none"),
	FUNCTION("function");

	private final String js;

	private CurveType(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
