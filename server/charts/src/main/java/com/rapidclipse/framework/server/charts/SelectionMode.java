
package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public enum SelectionMode implements JavaScriptable
{
	SINGLE("single"),
	MULTIPLE("multiple");
	
	private final String js;
	
	private SelectionMode(final String js)
	{
		this.js = Json.create(js).toJson();
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
