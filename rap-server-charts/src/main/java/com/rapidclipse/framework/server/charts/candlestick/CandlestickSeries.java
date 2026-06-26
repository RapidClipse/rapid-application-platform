/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.candlestick;

import com.rapidclipse.framework.server.charts.CandlestickColor;
import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface CandlestickSeries extends Series
{
	public String color();

	public CandlestickColor fallingColor();

	public Boolean labelInLegend();

	public CandlestickColor risingColor();

	public Integer targetAxisIndex();

	public Boolean visibleInLegend();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder color(String color);

		public Builder fallingColor(CandlestickColor fallingColor);

		public Builder labelInLegend(Boolean labelInLegend);

		public Builder risingColor(CandlestickColor risingColor);

		public Builder targetAxisIndex(Integer targetAxisIndex);

		public Builder visibleInLegend(Boolean visibleInLegend);

		public CandlestickSeries build();

		public static class Default implements Builder
		{
			private String           color;
			private CandlestickColor fallingColor;
			private Boolean          labelInLegend;
			private CandlestickColor risingColor;
			private Integer          targetAxisIndex;
			private Boolean          visibleInLegend;

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
			public Builder visibleInLegend(final Boolean visibleInLegend)
			{
				this.visibleInLegend = visibleInLegend;
				return this;
			}

			@Override
			public CandlestickSeries build()
			{
				return new CandlestickSeries.Default(this.color, this.fallingColor, this.labelInLegend,
					this.risingColor,
					this.targetAxisIndex, this.visibleInLegend);
			}
			
		}
		
	}

	public static class Default implements CandlestickSeries
	{
		private final String           color;
		private final CandlestickColor fallingColor;
		private final Boolean          labelInLegend;
		private final CandlestickColor risingColor;
		private final Integer          targetAxisIndex;
		private final Boolean          visibleInLegend;

		Default(
			final String color,
			final CandlestickColor fallingColor,
			final Boolean labelInLegend,
			final CandlestickColor risingColor,
			final Integer targetAxisIndex,
			final Boolean visibleInLegend)
		{
			super();

			this.color           = color;
			this.fallingColor    = fallingColor;
			this.labelInLegend   = labelInLegend;
			this.risingColor     = risingColor;
			this.targetAxisIndex = targetAxisIndex;
			this.visibleInLegend = visibleInLegend;
		}

		@Override
		public String color()
		{
			return this.color;
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
		public Boolean visibleInLegend()
		{
			return this.visibleInLegend;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("fallingColor", this.fallingColor);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("risingColor", this.risingColor);
			obj.putIfNotNull("targetAxisIndex", this.targetAxisIndex);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}

	}

}
