
package com.rapidclipse.framework.server.charts.table;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum ActionMode implements JavaScriptable
{
	ENABLE("enable"),
	EVENT("event"),
	DISABLE("disable");

	private final String js;

	private ActionMode(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
