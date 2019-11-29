
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Objects;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Tick extends Serializable, JavaScriptable
{
	public Object value();

	public String formattedValue();

	public static Tick New(final Object value)
	{
		return new Default(value, null);
	}

	public static Tick New(final Object value, final String formattedValue)
	{
		return new Default(value, formattedValue);
	}

	public static class Default implements Tick
	{
		private final Object value;
		private final String formattedValue;

		Default(final Object value, final String formattedValue)
		{
			super();

			this.value          = Static.validateValue(Objects.requireNonNull(value));
			this.formattedValue = formattedValue;
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
		public String js()
		{
			if(this.formattedValue == null)
			{
				return Static.js(this.value);
			}

			final ObjectHelper obj = new ObjectHelper();
			obj.putJson("v", Static.js(this.value));
			obj.putIfNotNull("f", this.formattedValue);
			return obj.js();
		}

	}

}
