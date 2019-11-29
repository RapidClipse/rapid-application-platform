
package com.rapidclipse.framework.server.charts.scatter;

import com.rapidclipse.framework.server.charts.PointShape;
import com.rapidclipse.framework.server.charts.PointShape.Type;
import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 *
 */
public interface ScatterSeries extends Series
{
	public String color();
	
	public Boolean labelInLegend();
	
	public Double lineWidth();
	
	public PointShape.Type pointShape();
	
	public Double pointSize();
	
	public Boolean pointsVisible();
	
	public Boolean visibleInLegend();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder color(String color);
		
		public Builder labelInLegend(Boolean labelInLegend);
		
		public Builder lineWidth(Double lineWidth);
		
		public Builder pointShape(PointShape.Type pointShape);
		
		public Builder pointSize(Double pointSize);
		
		public Builder pointsVisible(Boolean pointsVisible);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public ScatterSeries build();
		
		public static class Default implements Builder
		{
			private String          color;
			private Boolean         labelInLegend;
			private Double          lineWidth;
			private PointShape.Type pointShape;
			private Double          pointSize;
			private Boolean         pointsVisible;
			private Boolean         visibleInLegend;
			
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
			public Builder labelInLegend(final Boolean labelInLegend)
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
			public Builder pointShape(final PointShape.Type pointShape)
			{
				this.pointShape = pointShape;
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
			public Builder visibleInLegend(final Boolean visibleInLegend)
			{
				this.visibleInLegend = visibleInLegend;
				return this;
			}
			
			@Override
			public ScatterSeries build()
			{
				return new ScatterSeries.Default(this.color, this.labelInLegend, this.lineWidth, this.pointShape,
					this.pointSize, this.pointsVisible, this.visibleInLegend);
			}
			
		}
		
	}
	
	public static class Default implements ScatterSeries
	{
		private final String          color;
		private final Boolean         labelInLegend;
		private final Double          lineWidth;
		private final PointShape.Type pointShape;
		private final Double          pointSize;
		private final Boolean         pointsVisible;
		private final Boolean         visibleInLegend;
		
		Default(
			final String color,
			final Boolean labelInLegend,
			final Double lineWidth,
			final Type pointShape,
			final Double pointSize,
			final Boolean pointsVisible,
			final Boolean visibleInLegend)
		{
			super();
			
			this.color           = color;
			this.labelInLegend   = labelInLegend;
			this.lineWidth       = lineWidth;
			this.pointShape      = pointShape;
			this.pointSize       = pointSize;
			this.pointsVisible   = pointsVisible;
			this.visibleInLegend = visibleInLegend;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Boolean labelInLegend()
		{
			return this.labelInLegend;
		}
		
		@Override
		public Double lineWidth()
		{
			return this.lineWidth;
		}
		
		@Override
		public PointShape.Type pointShape()
		{
			return this.pointShape;
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
		public Boolean visibleInLegend()
		{
			return this.visibleInLegend;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("pointShape", this.pointShape);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("pointsVisible", this.pointsVisible);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}
		
	}
	
}
