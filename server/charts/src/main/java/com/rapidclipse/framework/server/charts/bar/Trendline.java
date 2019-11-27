
package com.rapidclipse.framework.server.charts.bar;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Trendline extends Serializable, JavaScriptable
{
	public static enum Type implements JavaScriptable
	{
		LINEAR("linear"),
		EXPONENTIAL("exponential"),
		POLYNOMIAL("polynomial");
		
		private final String js;
		
		private Type(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public String color();
	
	public Double degree();
	
	public String labelInLegend();
	
	public Double lineWidth();
	
	public Double opacity();
	
	public Double pointSize();
	
	public Boolean pointsVisible();
	
	public Boolean showR2();
	
	public Type type();
	
	public Boolean visibleInLegend();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder color(String color);
		
		public Builder degree(Double degree);
		
		public Builder labelInLegend(String labelInLegend);
		
		public Builder lineWidth(Double lineWidth);
		
		public Builder opacity(Double opacity);
		
		public Builder pointSize(Double pointSize);
		
		public Builder pointsVisible(Boolean pointsVisible);
		
		public Builder showR2(Boolean showR2);
		
		public Builder type(Type type);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public Trendline build();
		
		public static class Default implements Builder
		{
			private String  color;
			private Double  degree;
			private String  labelInLegend;
			private Double  lineWidth;
			private Double  opacity;
			private Double  pointSize;
			private Boolean pointsVisible;
			private Boolean showR2;
			private Type    type;
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
			public Builder degree(final Double degree)
			{
				this.degree = degree;
				return this;
			}
			
			@Override
			public Builder labelInLegend(final String labelInLegend)
			{
				this.labelInLegend = labelInLegend;
				return this;
			}
			
			@Override
			public Builder lineWidth(final Double lineWidth)
			{
				this.lineWidth = lineWidth;
				return this;
			}
			
			@Override
			public Builder opacity(final Double opacity)
			{
				this.opacity = opacity;
				return this;
			}
			
			@Override
			public Builder pointSize(final Double pointSize)
			{
				this.pointSize = pointSize;
				return this;
			}
			
			@Override
			public Builder pointsVisible(final Boolean pointsVisible)
			{
				this.pointsVisible = pointsVisible;
				return this;
			}
			
			@Override
			public Builder showR2(final Boolean showR2)
			{
				this.showR2 = showR2;
				return this;
			}
			
			@Override
			public Builder type(final Type type)
			{
				this.type = type;
				return this;
			}
			
			@Override
			public Builder visibleInLegend(final Boolean visibleInLegend)
			{
				this.visibleInLegend = visibleInLegend;
				return this;
			}
			
			@Override
			public Trendline build()
			{
				return new Trendline.Default(this.color, this.degree, this.labelInLegend, this.lineWidth, this.opacity,
					this.pointSize, this.pointsVisible,
					this.showR2, this.type, this.visibleInLegend);
			}

		}

	}
	
	public static class Default implements Trendline
	{
		private final String  color;
		private final Double  degree;
		private final String  labelInLegend;
		private final Double  lineWidth;
		private final Double  opacity;
		private final Double  pointSize;
		private final Boolean pointsVisible;
		private final Boolean showR2;
		private final Type    type;
		private final Boolean visibleInLegend;
		
		Default(
			final String color,
			final Double degree,
			final String labelInLegend,
			final Double lineWidth,
			final Double opacity,
			final Double pointSize,
			final Boolean pointsVisible,
			final Boolean showR2,
			final Type type,
			final Boolean visibleInLegend)
		{
			super();
			
			this.color           = color;
			this.degree          = degree;
			this.labelInLegend   = labelInLegend;
			this.lineWidth       = lineWidth;
			this.opacity         = opacity;
			this.pointSize       = pointSize;
			this.pointsVisible   = pointsVisible;
			this.showR2          = showR2;
			this.type            = type;
			this.visibleInLegend = visibleInLegend;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Double degree()
		{
			return this.degree;
		}
		
		@Override
		public String labelInLegend()
		{
			return this.labelInLegend;
		}
		
		@Override
		public Double lineWidth()
		{
			return this.lineWidth;
		}
		
		@Override
		public Double opacity()
		{
			return this.opacity;
		}
		
		@Override
		public Double pointSize()
		{
			return this.pointSize;
		}
		
		@Override
		public Boolean pointsVisible()
		{
			return this.pointsVisible;
		}
		
		@Override
		public Boolean showR2()
		{
			return this.showR2;
		}
		
		@Override
		public Type type()
		{
			return this.type;
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
			obj.putIfNotNull("degree", this.degree);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("opacity", this.opacity);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("pointsVisible", this.pointsVisible);
			obj.putIfNotNull("showR2", this.showR2);
			obj.putIfNotNull("type", this.type);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}

	}

}
