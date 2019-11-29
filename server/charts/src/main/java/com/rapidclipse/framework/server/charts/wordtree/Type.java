
package com.rapidclipse.framework.server.charts.wordtree;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public enum Type implements JavaScriptable
{
	PREFIX("prefix"),
	SUFFIX("suffix"),
	DOUBLE("double");

	private final String js;

	private Type(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
