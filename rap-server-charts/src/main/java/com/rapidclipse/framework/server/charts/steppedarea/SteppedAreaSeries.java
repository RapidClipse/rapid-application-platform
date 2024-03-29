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
package com.rapidclipse.framework.server.charts.steppedarea;

import java.util.Collections;
import java.util.List;

import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface SteppedAreaSeries extends Series
{
	public Number areaOpacity();
	
	public String color();
	
	public Boolean labelInLegend();
	
	public List<Number> lineDashStyle();

	public Integer targetAxisIndex();
	
	public Boolean visibleInLegend();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder areaOpacity(Number areaOpacity);
		
		public Builder color(String color);
		
		public Builder labelInLegend(Boolean labelInLegend);
		
		public Builder lineDashStyle(List<Number> lineDashStyle);
		
		public Builder targetAxisIndex(Integer targetAxisIndex);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public SteppedAreaSeries build();
		
		public static class Default implements Builder
		{
			private Number       areaOpacity;
			private String       color;
			private Boolean      labelInLegend;
			private List<Number> lineDashStyle;
			private Integer      targetAxisIndex;
			private Boolean      visibleInLegend;
			
			Default()
			{
				super();
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
			public SteppedAreaSeries build()
			{
				return new SteppedAreaSeries.Default(this.areaOpacity, this.color, this.labelInLegend,
					this.lineDashStyle, this.targetAxisIndex, this.visibleInLegend);
			}

		}

	}
	
	public static class Default implements SteppedAreaSeries
	{
		private final Number       areaOpacity;
		private final String       color;
		private final Boolean      labelInLegend;
		private final List<Number> lineDashStyle;
		private final Integer      targetAxisIndex;
		private final Boolean      visibleInLegend;

		Default(
			final Number areaOpacity,
			final String color,
			final Boolean labelInLegend,
			final List<Number> lineDashStyle,
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
