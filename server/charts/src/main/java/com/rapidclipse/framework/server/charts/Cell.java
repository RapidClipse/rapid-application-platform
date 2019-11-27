
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
public interface Cell extends Serializable, JavaScriptable
{
	public Object value();

	public String formattedValue();

	public JsonObject properties();

	public static Cell New(final Object value)
	{
		return new Default(value, null, null);
	}

	public static Cell New(final Object value, final String formattedValue)
	{
		return new Default(value, formattedValue, null);
	}

	public static Cell New(final Object value, final JsonObject properties)
	{
		return new Default(value, null, properties);
	}

	public static Cell New(final Object value, final String formattedValue, final JsonObject properties)
	{
		return new Default(value, formattedValue, properties);
	}

	public static class Default implements Cell
	{
		private final Object     value;
		private final String     formattedValue;
		private final JsonObject properties;

		Default(final Object value, final String formattedValue, final JsonObject properties)
		{
			super();

			this.value          = Static.validateValue(value);
			this.formattedValue = formattedValue;
			this.properties     = properties;
		}

		@Override
		public Object value()
		{
			return this.value;
		}

		@Override
		public String formattedValue()
		{
			return this.formattedValue;
		}

		@Override
		public JsonObject properties()
		{
			return this.properties;
		}

		@Override
		public String js()
		{
			if(this.formattedValue == null && this.properties == null)
			{
				return Static.js(this.value);
			}

			final ObjectHelper obj = new ObjectHelper();
			if(this.value != null)
			{
				obj.putJson("v", Static.js(this.value));
			}
			obj.putIfNotNull("f", this.formattedValue);
			obj.putIfNotNull("p", this.properties);
			return obj.js();
		}

	}

}