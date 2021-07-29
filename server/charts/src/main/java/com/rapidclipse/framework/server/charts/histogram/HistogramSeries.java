/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.histogram;

import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HistogramSeries extends Series
{
	public String color();

	public Boolean labelInLegend();

	public Integer targetAxisIndex();

	public Boolean visibleInLegend();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder color(String color);

		public Builder labelInLegend(Boolean labelInLegend);

		public Builder targetAxisIndex(Integer targetAxisIndex);

		public Builder visibleInLegend(Boolean visibleInLegend);

		public HistogramSeries build();

		public static class Default implements Builder
		{
			private String  color;
			private Boolean labelInLegend;
			private Integer targetAxisIndex;
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
			public Builder labelInLegend(final Boolean labelInLegend)
			{
				this.labelInLegend = labelInLegend;
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
			public HistogramSeries build()
			{
				return new HistogramSeries.Default(this.color, this.labelInLegend, this.targetAxisIndex,
					this.visibleInLegend);
			}

		}

	}

	public static class Default implements HistogramSeries
	{
		private final String  color;
		private final Boolean labelInLegend;
		private final Integer targetAxisIndex;
		private final Boolean visibleInLegend;

		Default(
			final String color,
			final Boolean labelInLegend,
			final Integer targetAxisIndex,
			final Boolean visibleInLegend)
		{
			super();

			this.color           = color;
			this.labelInLegend   = labelInLegend;
			this.targetAxisIndex = targetAxisIndex;
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
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("targetAxisIndex", this.targetAxisIndex);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}

	}

}
