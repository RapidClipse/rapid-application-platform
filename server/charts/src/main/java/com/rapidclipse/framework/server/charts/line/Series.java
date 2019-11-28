
package com.rapidclipse.framework.server.charts.line;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import com.rapidclipse.framework.server.charts.Annotations;
import com.rapidclipse.framework.server.charts.CurveType;
import com.rapidclipse.framework.server.charts.PointShape;
import com.rapidclipse.framework.server.charts.PointShape.Type;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Series extends Serializable, JavaScriptable
{
	public Annotations annotations();
	
	public String color();
	
	public CurveType curveType();
	
	public Boolean labelInLegend();
	
	public List<Double> lineDashStyle();
	
	public Double lineWidth();
	
	public PointShape.Type pointShape();
	
	public Double pointSize();
	
	public Boolean pointsVisible();
	
	public Integer targetAxisIndex();
	
	public Boolean visibleInLegend();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder annotations(Annotations annotations);
		
		public Builder color(String color);
		
		public Builder curveType(CurveType curveType);
		
		public Builder labelInLegend(Boolean labelInLegend);
		
		public Builder lineDashStyle(List<Double> lineDashStyle);
		
		public Builder lineWidth(Double lineWidth);
		
		public Builder pointShape(PointShape.Type pointShape);
		
		public Builder pointSize(Double pointSize);
		
		public Builder pointsVisible(Boolean pointsVisible);
		
		public Builder targetAxisIndex(Integer targetAxisIndex);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public Series build();
		
		public static class Default implements Builder
		{
			private Annotations     annotations;
			private String          color;
			private CurveType       curveType;
			private Boolean         labelInLegend;
			private List<Double>    lineDashStyle;
			private Double          lineWidth;
			private PointShape.Type pointShape;
			private Double          pointSize;
			private Boolean         pointsVisible;
			private Integer         targetAxisIndex;
			private Boolean         visibleInLegend;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder annotations(final Annotations annotations)
			{
				this.annotations = annotations;
				return this;
			}
			
			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}
			
			@Override
			public Builder curveType(final CurveType curveType)
			{
				this.curveType = curveType;
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
				return new Series.Default(this.annotations, this.color, this.curveType, this.labelInLegend,
					this.lineDashStyle, this.lineWidth, this.pointShape, this.pointSize, this.pointsVisible,
					this.targetAxisIndex, this.visibleInLegend);
			}
			
		}
		
	}
	
	public static class Default implements Series
	{
		private final Annotations     annotations;
		private final String          color;
		private final CurveType       curveType;
		private final Boolean         labelInLegend;
		private final List<Double>    lineDashStyle;
		private final Double          lineWidth;
		private final PointShape.Type pointShape;
		private final Double          pointSize;
		private final Boolean         pointsVisible;
		private final Integer         targetAxisIndex;
		private final Boolean         visibleInLegend;
		
		Default(
			final Annotations annotations,
			final String color,
			final CurveType curveType,
			final Boolean labelInLegend,
			final List<Double> lineDashStyle,
			final Double lineWidth,
			final Type pointShape,
			final Double pointSize,
			final Boolean pointsVisible,
			final Integer targetAxisIndex,
			final Boolean visibleInLegend)
		{
			super();
			
			this.annotations     = annotations;
			this.color           = color;
			this.curveType       = curveType;
			this.labelInLegend   = labelInLegend;
			this.lineDashStyle   = lineDashStyle != null ? Collections.unmodifiableList(lineDashStyle) : null;
			this.lineWidth       = lineWidth;
			this.pointShape      = pointShape;
			this.pointSize       = pointSize;
			this.pointsVisible   = pointsVisible;
			this.targetAxisIndex = targetAxisIndex;
			this.visibleInLegend = visibleInLegend;
		}
		
		@Override
		public Annotations annotations()
		{
			return this.annotations;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public CurveType curveType()
		{
			return this.curveType;
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
			obj.putIfNotNull("annotations", this.annotations);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("curveType", this.curveType);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("lineDashStyle", new ArrayHelper().addAllNumbers(this.lineDashStyle));
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("pointShape", this.pointShape);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("pointsVisible", this.pointsVisible);
			obj.putIfNotNull("targetAxisIndex", this.targetAxisIndex);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}
		
	}
	
}
