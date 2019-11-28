
package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface MagnifyingGlass extends Serializable, JavaScriptable
{
	public Boolean enable();

	public Double zoomFactor();

	public static MagnifyingGlass New(final Boolean enable, final Double zoomFactor)
	{
		return new Default(enable, zoomFactor);
	}

	public static class Default implements MagnifyingGlass
	{
		private final Boolean enable;
		private final Double  zoomFactor;

		Default(final Boolean enable, final Double zoomFactor)
		{
			super();

			this.enable     = enable;
			this.zoomFactor = zoomFactor;
		}

		@Override
		public Boolean enable()
		{
			return this.enable;
		}

		@Override
		public Double zoomFactor()
		{
			return this.zoomFactor;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("enable", this.enable);
			obj.putIfNotNull("zoomFactor", this.zoomFactor);
			return obj.js();
		}

	}

}
