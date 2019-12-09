/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.calendar;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Calendar extends Serializable, JavaScriptable
{
	public CellColor cellColor();

	public Integer cellSize();

	public TextStyle dayOfWeekLabel();

	public Integer dayOfWeekRightSpace();

	public String daysOfWeek();

	public CellColor focusedCellColor();

	public TextStyle monthLabel();

	public CellColor monthOutlineColor();

	public Integer underMonthSpace();

	public Integer underYearSpace();

	public CellColor unusedMonthOutlineColor();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder cellColor(CellColor cellColor);

		public Builder cellSize(Integer cellSize);

		public Builder dayOfWeekLabel(TextStyle dayOfWeekLabel);

		public Builder dayOfWeekRightSpace(Integer dayOfWeekRightSpace);

		public Builder daysOfWeek(String daysOfWeek);

		public Builder focusedCellColor(CellColor focusedCellColor);

		public Builder monthLabel(TextStyle monthLabel);

		public Builder monthOutlineColor(CellColor monthOutlineColor);

		public Builder underMonthSpace(Integer underMonthSpace);

		public Builder underYearSpace(Integer underYearSpace);

		public Builder unusedMonthOutlineColor(CellColor unusedMonthOutlineColor);

		public Calendar build();

		public static class Default implements Builder
		{
			private CellColor cellColor;
			private Integer   cellSize;
			private TextStyle dayOfWeekLabel;
			private Integer   dayOfWeekRightSpace;
			private String    daysOfWeek;
			private CellColor focusedCellColor;
			private TextStyle monthLabel;
			private CellColor monthOutlineColor;
			private Integer   underMonthSpace;
			private Integer   underYearSpace;
			private CellColor unusedMonthOutlineColor;

			Default()
			{
				super();
			}

			@Override
			public Builder cellColor(final CellColor cellColor)
			{
				this.cellColor = cellColor;
				return this;
			}

			@Override
			public Builder cellSize(final Integer cellSize)
			{
				this.cellSize = cellSize;
				return this;
			}

			@Override
			public Builder dayOfWeekLabel(final TextStyle dayOfWeekLabel)
			{
				this.dayOfWeekLabel = dayOfWeekLabel;
				return this;
			}

			@Override
			public Builder dayOfWeekRightSpace(final Integer dayOfWeekRightSpace)
			{
				this.dayOfWeekRightSpace = dayOfWeekRightSpace;
				return this;
			}

			@Override
			public Builder daysOfWeek(final String daysOfWeek)
			{
				this.daysOfWeek = daysOfWeek;
				return this;
			}

			@Override
			public Builder focusedCellColor(final CellColor focusedCellColor)
			{
				this.focusedCellColor = focusedCellColor;
				return this;
			}

			@Override
			public Builder monthLabel(final TextStyle monthLabel)
			{
				this.monthLabel = monthLabel;
				return this;
			}

			@Override
			public Builder monthOutlineColor(final CellColor monthOutlineColor)
			{
				this.monthOutlineColor = monthOutlineColor;
				return this;
			}

			@Override
			public Builder underMonthSpace(final Integer underMonthSpace)
			{
				this.underMonthSpace = underMonthSpace;
				return this;
			}

			@Override
			public Builder underYearSpace(final Integer underYearSpace)
			{
				this.underYearSpace = underYearSpace;
				return this;
			}

			@Override
			public Builder unusedMonthOutlineColor(final CellColor unusedMonthOutlineColor)
			{
				this.unusedMonthOutlineColor = unusedMonthOutlineColor;
				return this;
			}

			@Override
			public Calendar build()
			{
				return new Calendar.Default(this.cellColor, this.cellSize, this.dayOfWeekLabel,
					this.dayOfWeekRightSpace, this.daysOfWeek,
					this.focusedCellColor, this.monthLabel, this.monthOutlineColor, this.underMonthSpace,
					this.underYearSpace,
					this.unusedMonthOutlineColor);
			}
			
		}
		
	}

	public static class Default implements Calendar
	{
		private final CellColor cellColor;
		private final Integer   cellSize;
		private final TextStyle dayOfWeekLabel;
		private final Integer   dayOfWeekRightSpace;
		private final String    daysOfWeek;
		private final CellColor focusedCellColor;
		private final TextStyle monthLabel;
		private final CellColor monthOutlineColor;
		private final Integer   underMonthSpace;
		private final Integer   underYearSpace;
		private final CellColor unusedMonthOutlineColor;

		Default(
			final CellColor cellColor,
			final Integer cellSize,
			final TextStyle dayOfWeekLabel,
			final Integer dayOfWeekRightSpace,
			final String daysOfWeek,
			final CellColor focusedCellColor,
			final TextStyle monthLabel,
			final CellColor monthOutlineColor,
			final Integer underMonthSpace,
			final Integer underYearSpace,
			final CellColor unusedMonthOutlineColor)
		{
			super();

			this.cellColor               = cellColor;
			this.cellSize                = cellSize;
			this.dayOfWeekLabel          = dayOfWeekLabel;
			this.dayOfWeekRightSpace     = dayOfWeekRightSpace;
			this.daysOfWeek              = daysOfWeek;
			this.focusedCellColor        = focusedCellColor;
			this.monthLabel              = monthLabel;
			this.monthOutlineColor       = monthOutlineColor;
			this.underMonthSpace         = underMonthSpace;
			this.underYearSpace          = underYearSpace;
			this.unusedMonthOutlineColor = unusedMonthOutlineColor;
		}

		@Override
		public CellColor cellColor()
		{
			return this.cellColor;
		}

		@Override
		public Integer cellSize()
		{
			return this.cellSize;
		}

		@Override
		public TextStyle dayOfWeekLabel()
		{
			return this.dayOfWeekLabel;
		}

		@Override
		public Integer dayOfWeekRightSpace()
		{
			return this.dayOfWeekRightSpace;
		}

		@Override
		public String daysOfWeek()
		{
			return this.daysOfWeek;
		}

		@Override
		public CellColor focusedCellColor()
		{
			return this.focusedCellColor;
		}

		@Override
		public TextStyle monthLabel()
		{
			return this.monthLabel;
		}

		@Override
		public CellColor monthOutlineColor()
		{
			return this.monthOutlineColor;
		}

		@Override
		public Integer underMonthSpace()
		{
			return this.underMonthSpace;
		}

		@Override
		public Integer underYearSpace()
		{
			return this.underYearSpace;
		}

		@Override
		public CellColor unusedMonthOutlineColor()
		{
			return this.unusedMonthOutlineColor;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("cellColor", this.cellColor);
			obj.putIfNotNull("cellSize", this.cellSize);
			obj.putIfNotNull("dayOfWeekLabel", this.dayOfWeekLabel);
			obj.putIfNotNull("dayOfWeekRightSpace", this.dayOfWeekRightSpace);
			obj.putIfNotNull("daysOfWeek", this.daysOfWeek);
			obj.putIfNotNull("focusedCellColor", this.focusedCellColor);
			obj.putIfNotNull("monthLabel", this.monthLabel);
			obj.putIfNotNull("monthOutlineColor", this.monthOutlineColor);
			obj.putIfNotNull("underMonthSpace", this.underMonthSpace);
			obj.putIfNotNull("underYearSpace", this.underYearSpace);
			obj.putIfNotNull("unusedMonthOutlineColor", this.unusedMonthOutlineColor);
			return obj.js();
		}

	}

}
