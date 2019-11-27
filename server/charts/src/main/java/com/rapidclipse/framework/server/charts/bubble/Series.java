
package com.rapidclipse.framework.server.charts.bubble;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Series extends Serializable, JavaScriptable
{
	public String color();

	public Boolean visibleInLegend();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder color(String color);

		public Builder visibleInLegend(Boolean visibleInLegend);

		public Series build();

		public static class Default implements Builder
		{
			private String  color;
			private Boolean visibleInLegend;

			Default()
			{
				super();
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder visibleInLegend(final Boolean visibleInLegend)
			{
				this.visibleInLegend = visibleInLegend;
				return this;
			}

			@Override
			public Series build()
			{
				return new Series.Default(this.color, this.visibleInLegend);
			}
			
		}
		
	}

	public static class Default implements Series
	{
		private final String  color;
		private final Boolean visibleInLegend;
		
		Default(
			final String color,
			final Boolean visibleInLegend)
		{
			super();

			this.color           = color;
			this.visibleInLegend = visibleInLegend;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Boolean visibleInLegend()
		{
			return this.visibleInLegend;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}

	}

}