/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.area;

import java.util.Collections;
import java.util.List;

import com.rapidclipse.framework.server.charts.Annotations;
import com.rapidclipse.framework.server.charts.PointShape;
import com.rapidclipse.framework.server.charts.PointShape.Type;
import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface AreaSeries extends Series
{
	public Annotations annotations();

	public Number areaOpacity();

	public String color();

	public Boolean labelInLegend();

	public List<Number> lineDashStyle();

	public Number lineWidth();

	public PointShape.Type pointShape();

	public Number pointSize();

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

		public Builder areaOpacity(Number areaOpacity);

		public Builder color(String color);

		public Builder labelInLegend(Boolean labelInLegend);

		public Builder lineDashStyle(List<Number> lineDashStyle);

		public Builder lineWidth(Number lineWidth);

		public Builder pointShape(PointShape.Type pointShape);

		public Builder pointSize(Number pointSize);

		public Builder pointsVisible(Boolean pointsVisible);

		public Builder targetAxisIndex(Integer targetAxisIndex);

		public Builder visibleInLegend(Boolean visibleInLegend);

		public AreaSeries build();

		public static class Default implements Builder
		{
			private Annotations     annotations;
			private Number          areaOpacity;
			private String          color;
			private Boolean         labelInLegend;
			private List<Number>    lineDashStyle;
			private Number          lineWidth;
			private PointShape.Type pointShape;
			private Number          pointSize;
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
			public Builder areaOpacity(final Number areaOpacity)
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
			public Builder lineDashStyle(final List<Number> lineDashStyle)
			{
				this.lineDashStyle = lineDashStyle;
				return this;
			}

			@Override
			public Builder lineWidth(final Number lineWidth)
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
			public Builder pointSize(final Number pointSize)
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
			public AreaSeries build()
			{
				return new AreaSeries.Default(this.annotations, this.areaOpacity, this.color, this.labelInLegend,
					this.lineDashStyle, this.lineWidth, this.pointShape, this.pointSize, this.pointsVisible,
					this.targetAxisIndex, this.visibleInLegend);
			}
			
		}
		
	}

	public static class Default implements AreaSeries
	{
		private final Annotations     annotations;
		private final Number          areaOpacity;
		private final String          color;
		private final Boolean         labelInLegend;
		private final List<Number>    lineDashStyle;
		private final Number          lineWidth;
		private final PointShape.Type pointShape;
		private final Number          pointSize;
		private final Boolean         pointsVisible;
		private final Integer         targetAxisIndex;
		private final Boolean         visibleInLegend;
		
		Default(
			final Annotations annotations,
			final Number areaOpacity,
			final String color,
			final Boolean labelInLegend,
			final List<Number> lineDashStyle,
			final Number lineWidth,
			final Type pointShape,
			final Number pointSize,
			final Boolean pointsVisible,
			final Integer targetAxisIndex,
			final Boolean visibleInLegend)
		{
			super();

			this.annotations     = annotations;
			this.areaOpacity     = areaOpacity;
			this.color           = color;
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
		public Number areaOpacity()
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
		public List<Number> lineDashStyle()
		{
			return this.lineDashStyle;
		}

		@Override
		public Number lineWidth()
		{
			return this.lineWidth;
		}

		@Override
		public PointShape.Type pointShape()
		{
			return this.pointShape;
		}

		@Override
		public Number pointSize()
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
			obj.putIfNotNull("areaOpacity", this.areaOpacity);
			obj.putIfNotNull("color", this.color);
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
