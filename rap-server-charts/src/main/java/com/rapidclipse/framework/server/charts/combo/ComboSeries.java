/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.combo;

import java.util.Collections;
import java.util.List;

import com.rapidclipse.framework.server.charts.Annotations;
import com.rapidclipse.framework.server.charts.CandlestickColor;
import com.rapidclipse.framework.server.charts.CurveType;
import com.rapidclipse.framework.server.charts.PointShape;
import com.rapidclipse.framework.server.charts.PointShape.Type;
import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface ComboSeries extends Series
{
	public Annotations annotations();

	public Number areaOpacity();

	public String color();

	public CurveType curveType();

	public CandlestickColor fallingColor();

	public Boolean labelInLegend();

	public List<Number> lineDashStyle();

	public Number lineWidth();

	public PointShape.Type pointShape();

	public Number pointSize();

	public Boolean pointsVisible();

	public CandlestickColor risingColor();

	public Integer targetAxisIndex();

	public SeriesType type();

	public Boolean visibleInLegend();

	public static ComboSeries New(final SeriesType seriesType)
	{
		return Builder().type(seriesType).build();
	}

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder annotations(Annotations annotations);

		public Builder areaOpacity(Number areaOpacity);

		public Builder curveType(CurveType curveType);

		public Builder fallingColor(CandlestickColor fallingColor);

		public Builder color(String color);

		public Builder labelInLegend(Boolean labelInLegend);

		public Builder lineDashStyle(List<Number> lineDashStyle);

		public Builder lineWidth(Number lineWidth);

		public Builder pointShape(PointShape.Type pointShape);

		public Builder pointSize(Number pointSize);

		public Builder pointsVisible(Boolean pointsVisible);

		public Builder risingColor(CandlestickColor risingColor);

		public Builder targetAxisIndex(Integer targetAxisIndex);

		public Builder type(SeriesType type);

		public Builder visibleInLegend(Boolean visibleInLegend);

		public ComboSeries build();

		public static class Default implements Builder
		{
			private Annotations      annotations;
			private Number           areaOpacity;
			private String           color;
			private CurveType        curveType;
			private CandlestickColor fallingColor;
			private Boolean          labelInLegend;
			private List<Number>     lineDashStyle;
			private Number           lineWidth;
			private PointShape.Type  pointShape;
			private Number           pointSize;
			private Boolean          pointsVisible;
			private CandlestickColor risingColor;
			private Integer          targetAxisIndex;
			private SeriesType       type;
			private Boolean          visibleInLegend;

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
			public Builder curveType(final CurveType curveType)
			{
				this.curveType = curveType;
				return this;
			}

			@Override
			public Builder fallingColor(final CandlestickColor fallingColor)
			{
				this.fallingColor = fallingColor;
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
			public Builder risingColor(final CandlestickColor risingColor)
			{
				this.risingColor = risingColor;
				return this;
			}

			@Override
			public Builder targetAxisIndex(final Integer targetAxisIndex)
			{
				this.targetAxisIndex = targetAxisIndex;
				return this;
			}

			@Override
			public Builder type(final SeriesType type)
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
			public ComboSeries build()
			{
				return new ComboSeries.Default(this.annotations, this.areaOpacity, this.color, this.curveType,
					this.fallingColor, this.labelInLegend, this.lineDashStyle, this.lineWidth, this.pointShape,
					this.pointSize, this.pointsVisible, this.risingColor, this.targetAxisIndex, this.type,
					this.visibleInLegend);
			}

		}

	}

	public static class Default implements ComboSeries
	{
		private final Annotations      annotations;
		private final Number           areaOpacity;
		private final String           color;
		private final CurveType        curveType;
		private final CandlestickColor fallingColor;
		private final Boolean          labelInLegend;
		private final List<Number>     lineDashStyle;
		private final Number           lineWidth;
		private final PointShape.Type  pointShape;
		private final Number           pointSize;
		private final Boolean          pointsVisible;
		private final CandlestickColor risingColor;
		private final Integer          targetAxisIndex;
		private final SeriesType       type;
		private final Boolean          visibleInLegend;

		Default(
			final Annotations annotations,
			final Number areaOpacity,
			final String color,
			final CurveType curveType,
			final CandlestickColor fallingColor,
			final Boolean labelInLegend,
			final List<Number> lineDashStyle,
			final Number lineWidth,
			final Type pointShape,
			final Number pointSize,
			final Boolean pointsVisible,
			final CandlestickColor risingColor,
			final Integer targetAxisIndex,
			final SeriesType type,
			final Boolean visibleInLegend)
		{
			super();

			this.annotations     = annotations;
			this.areaOpacity     = areaOpacity;
			this.color           = color;
			this.curveType       = curveType;
			this.fallingColor    = fallingColor;
			this.labelInLegend   = labelInLegend;
			this.lineDashStyle   = lineDashStyle != null ? Collections.unmodifiableList(lineDashStyle) : null;
			this.lineWidth       = lineWidth;
			this.pointShape      = pointShape;
			this.pointSize       = pointSize;
			this.pointsVisible   = pointsVisible;
			this.risingColor     = risingColor;
			this.targetAxisIndex = targetAxisIndex;
			this.type            = type;
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
		public CurveType curveType()
		{
			return this.curveType;
		}

		@Override
		public CandlestickColor fallingColor()
		{
			return this.fallingColor;
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
		public CandlestickColor risingColor()
		{
			return this.risingColor;
		}

		@Override
		public Integer targetAxisIndex()
		{
			return this.targetAxisIndex;
		}

		@Override
		public SeriesType type()
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
			obj.putIfNotNull("annotations", this.annotations);
			obj.putIfNotNull("areaOpacity", this.areaOpacity);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("curveType", this.curveType);
			obj.putIfNotNull("fallingColor", this.fallingColor);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("lineDashStyle", new ArrayHelper().addAllNumbers(this.lineDashStyle));
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("pointShape", this.pointShape);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("pointsVisible", this.pointsVisible);
			obj.putIfNotNull("risingColor", this.risingColor);
			obj.putIfNotNull("targetAxisIndex", this.targetAxisIndex);
			obj.putIfNotNull("type", this.type);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}

	}

}
