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
package com.rapidclipse.framework.server.charts.scatter;

import com.rapidclipse.framework.server.charts.PointShape;
import com.rapidclipse.framework.server.charts.PointShape.Type;
import com.rapidclipse.framework.server.charts.Series;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface ScatterSeries extends Series
{
	public String color();
	
	public Boolean labelInLegend();
	
	public Number lineWidth();
	
	public PointShape.Type pointShape();
	
	public Number pointSize();
	
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
		
		public Builder lineWidth(Number lineWidth);
		
		public Builder pointShape(PointShape.Type pointShape);
		
		public Builder pointSize(Number pointSize);
		
		public Builder pointsVisible(Boolean pointsVisible);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public ScatterSeries build();
		
		public static class Default implements Builder
		{
			private String          color;
			private Boolean         labelInLegend;
			private Number          lineWidth;
			private PointShape.Type pointShape;
			private Number          pointSize;
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
		private final Number          lineWidth;
		private final PointShape.Type pointShape;
		private final Number          pointSize;
		private final Boolean         pointsVisible;
		private final Boolean         visibleInLegend;
		
		Default(
			final String color,
			final Boolean labelInLegend,
			final Number lineWidth,
			final Type pointShape,
			final Number pointSize,
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
