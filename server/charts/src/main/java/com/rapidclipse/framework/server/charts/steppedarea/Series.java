
package com.rapidclipse.framework.server.charts.steppedarea;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Series extends Serializable, JavaScriptable
{
	public Double areaOpacity();
	
	public String color();
	
	public Boolean labelInLegend();
	
	public List<Double> lineDashStyle();

	public Integer targetAxisIndex();
	
	public Boolean visibleInLegend();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder areaOpacity(Double areaOpacity);
		
		public Builder color(String color);
		
		public Builder labelInLegend(Boolean labelInLegend);
		
		public Builder lineDashStyle(List<Double> lineDashStyle);
		
		public Builder targetAxisIndex(Integer targetAxisIndex);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public Series build();
		
		public static class Default implements Builder
		{
			private Double       areaOpacity;
			private String       color;
			private Boolean      labelInLegend;
			private List<Double> lineDashStyle;
			private Integer      targetAxisIndex;
			private Boolean      visibleInLegend;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder areaOpacity(final Double areaOpacity)
			{
				this.areaOpacity = areaOpacity;
				return this;
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
			public Builder lineDashStyle(final List<Double> lineDashStyle)
			{
				this.lineDashStyle = lineDashStyle;
				return this;
			}
			
			@Override
			public Builder targetAxisIndex(final Integer targetAxisIndex)
			{
				this.targetAxisIndex = targetAxisIndex;
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
				return new Series.Default(this.areaOpacity, this.color, this.labelInLegend,
					this.lineDashStyle, this.targetAxisIndex, this.visibleInLegend);
			}

		}

	}
	
	public static class Default implements Series
	{
		private final Double       areaOpacity;
		private final String       color;
		private final Boolean      labelInLegend;
		private final List<Double> lineDashStyle;
		private final Integer      targetAxisIndex;
		private final Boolean      visibleInLegend;

		Default(
			final Double areaOpacity,
			final String color,
			final Boolean labelInLegend,
			final List<Double> lineDashStyle,
			final Integer targetAxisIndex,
			final Boolean visibleInLegend)
		{
			super();
			
			this.areaOpacity     = areaOpacity;
			this.color           = color;
			this.labelInLegend   = labelInLegend;
			this.lineDashStyle   = lineDashStyle != null ? Collections.unmodifiableList(lineDashStyle) : null;
			this.targetAxisIndex = targetAxisIndex;
			this.visibleInLegend = visibleInLegend;
		}
		
		@Override
		public Double areaOpacity()
		{
			return this.areaOpacity;
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
		public List<Double> lineDashStyle()
		{
			return this.lineDashStyle;
		}
		
		@Override
		public Integer targetAxisIndex()
		{
			return this.targetAxisIndex;
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
			obj.putIfNotNull("areaOpacity", this.areaOpacity);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("lineDashStyle", new ArrayHelper().addAllNumbers(this.lineDashStyle));
			obj.putIfNotNull("targetAxisIndex", this.targetAxisIndex);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}
		
	}
	
}
