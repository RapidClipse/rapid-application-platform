
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Animation extends Serializable, JavaScriptable
{
	public static enum Easing implements JavaScriptable
	{
		LINEAR("linear"),
		IN("in"),
		OUT("out"),
		IN_AND_OUT("inAndOut");

		private final String js;

		private Easing(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Integer duration();

	public Easing easing();

	public Boolean startup();

	public static Animation New(final Integer duration)
	{
		return new Default(duration, null, null);
	}

	public static Animation New(final Easing easing)
	{
		return new Default(null, easing, null);
	}

	public static Animation New(final Integer duration, final Easing easing)
	{
		return new Default(duration, easing, null);
	}

	public static Animation New(final Integer duration, final Easing easing, final Boolean startup)
	{
		return new Default(duration, easing, startup);
	}

	public static class Default implements Animation
	{
		private final Integer duration;
		private final Easing  easing;
		private final Boolean startup;

		Default(final Integer duration, final Easing easing, final Boolean startup)
		{
			super();

			this.duration = duration;
			this.easing   = easing;
			this.startup  = startup;
		}

		@Override
		public Integer duration()
		{
			return this.duration;
		}

		@Override
		public Easing easing()
		{
			return this.easing;
		}

		@Override
		public Boolean startup()
		{
			return this.startup;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("duration", this.duration);
			obj.putIfNotNull("easing", this.easing);
			obj.putIfNotNull("startup", this.startup);
			return obj.js();
		}
	}
}
