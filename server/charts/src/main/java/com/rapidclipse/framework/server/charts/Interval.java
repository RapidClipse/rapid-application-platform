
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Interval extends Serializable, JavaScriptable
{
	public static enum Style implements JavaScriptable
	{
		LINE("line"),
		BAR("bar"),
		BOX("box"),
		STICK("stick"),
		POINT("point"),
		AREA("area");

		private final String js;

		private Style(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Style style();

	public String color();

	public Double lineWidth();

	public Double barWidth();

	public Double boxWidth();

	public Double pointSize();

	public Double fillOpacity();

	public CurveType curveType();

	public static Interval New(final Style style)
	{
		return Builder().style(style).build();
	}
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder style(Style style);

		public Builder color(String color);

		public Builder lineWidth(Double lineWidth);

		public Builder barWidth(Double barWidth);

		public Builder boxWidth(Double boxWidth);

		public Builder pointSize(Double pointSize);

		public Builder fillOpacity(Double fillOpacity);

		public Builder curveType(CurveType curveType);

		public Interval build();
		
		public static class Default implements Builder
		{
			private Style     style;
			private String    color;
			private Double    lineWidth;
			private Double    barWidth;
			private Double    boxWidth;
			private Double    pointSize;
			private Double    fillOpacity;
			private CurveType curveType;
			
			Default()
			{
				super();
			}

			@Override
			public Builder style(final Style style)
			{
				this.style = style;
				return this;
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder lineWidth(final Double lineWidth)
			{
				this.lineWidth = lineWidth;
				return this;
			}

			@Override
			public Builder barWidth(final Double barWidth)
			{
				this.barWidth = barWidth;
				return this;
			}

			@Override
			public Builder boxWidth(final Double boxWidth)
			{
				this.boxWidth = boxWidth;
				return this;
			}

			@Override
			public Builder pointSize(final Double pointSize)
			{
				this.pointSize = pointSize;
				return this;
			}

			@Override
			public Builder fillOpacity(final Double fillOpacity)
			{
				this.fillOpacity = fillOpacity;
				return this;
			}

			@Override
			public Builder curveType(final CurveType curveType)
			{
				this.curveType = curveType;
				return this;
			}

			@Override
			public Interval build()
			{
				return new Interval.Default(this.style, this.color, this.lineWidth, this.barWidth, this.boxWidth,
					this.pointSize, this.fillOpacity, this.curveType);
			}

		}

	}

	public static class Default implements Interval
	{
		private final Style     style;
		private final String    color;
		private final Double    lineWidth;
		private final Double    barWidth;
		private final Double    boxWidth;
		private final Double    pointSize;
		private final Double    fillOpacity;
		private final CurveType curveType;

		Default(
			final Style style,
			final String color,
			final Double lineWidth,
			final Double barWidth,
			final Double boxWidth,
			final Double pointSize,
			final Double fillOpacity,
			final CurveType curveType)
		{
			super();

			this.style       = style;
			this.color       = color;
			this.lineWidth   = lineWidth;
			this.barWidth    = barWidth;
			this.boxWidth    = boxWidth;
			this.pointSize   = pointSize;
			this.fillOpacity = fillOpacity;
			this.curveType   = curveType;
		}
		
		@Override
		public Style style()
		{
			return this.style;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Double lineWidth()
		{
			return this.lineWidth;
		}

		@Override
		public Double barWidth()
		{
			return this.barWidth;
		}

		@Override
		public Double boxWidth()
		{
			return this.boxWidth;
		}

		@Override
		public Double pointSize()
		{
			return this.pointSize;
		}

		@Override
		public Double fillOpacity()
		{
			return this.fillOpacity;
		}

		@Override
		public CurveType curveType()
		{
			return this.curveType;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("style", this.style);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("barWidth", this.barWidth);
			obj.putIfNotNull("boxWidth", this.boxWidth);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("fillOpacity", this.fillOpacity);
			obj.putIfNotNull("curveType", this.curveType);
			return obj.js();
		}

	}

}
