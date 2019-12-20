
package com.rapidclipse.framework.server.webapi;

import com.google.gson.Gson;

import elemental.json.JsonValue;
import elemental.json.impl.JreJsonFactory;


/**
 * Utility class that contains various convinience methods for working with vaadin's json values
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public final class JsonUtils
{
	public static final Gson GSON = new Gson();
	
	/**
	 * Convert an object into a {@link JsonValue}.
	 *
	 * @param obj
	 *            The object to be converted.
	 * @return The converted object.
	 */
	public static JsonValue encodeObject(final Object obj)
	{
		return new JreJsonFactory().parse(JsonUtils.GSON.toJson(obj));
	}
	
	private JsonUtils()
	{
		throw new Error();
	}
}