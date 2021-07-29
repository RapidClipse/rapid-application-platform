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
package com.rapidclipse.framework.server.charts.bubble;

import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface BubbleSeries extends Series
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

		public BubbleSeries build();

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
			public BubbleSeries build()
			{
				return new BubbleSeries.Default(this.color, this.visibleInLegend);
			}
			
		}
		
	}

	public static class Default implements BubbleSeries
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
