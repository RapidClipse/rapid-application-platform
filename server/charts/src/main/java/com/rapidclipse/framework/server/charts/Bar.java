
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Bar extends Serializable, JavaScriptable
{
	public Size groupWidth();

	public static Bar New(final Size groupWidth)
	{
		return new Default(groupWidth);
	}

	public static class Default implements Bar
	{
		private final Size groupWidth;

		Default(final Size groupWidth)
		{
			super();

			this.groupWidth = groupWidth;
		}

		@Override
		public Size groupWidth()
		{
			return this.groupWidth;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("groupWidth", this.groupWidth);
			return obj.js();
		}
	}
}
