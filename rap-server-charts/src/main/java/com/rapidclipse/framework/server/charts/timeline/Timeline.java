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
package com.rapidclipse.framework.server.charts.timeline;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;
import com.rapidclipse.framework.server.charts.Styles;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Timeline extends Serializable, JavaScriptable
{
	public Styles barLabelStyle();

	public Boolean colorByRowLabel();

	public Boolean groupByRowLabel();

	public Styles rowLabelStyle();

	public Boolean showRowLabels();

	public String singleColor();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder barLabelStyle(Styles barLabelStyle);

		public Builder colorByRowLabel(Boolean colorByRowLabel);

		public Builder groupByRowLabel(Boolean groupByRowLabel);

		public Builder rowLabelStyle(Styles rowLabelStyle);

		public Builder showRowLabels(Boolean showRowLabels);

		public Builder singleColor(String singleColor);

		public Timeline build();
		
		public static class Default implements Builder
		{
			private Styles  barLabelStyle;
			private Boolean colorByRowLabel;
			private Boolean groupByRowLabel;
			private Styles  rowLabelStyle;
			private Boolean showRowLabels;
			private String  singleColor;
			
			Default()
			{
				super();
			}

			@Override
			public Builder barLabelStyle(final Styles barLabelStyle)
			{
				this.barLabelStyle = barLabelStyle;
				return this;
			}

			@Override
			public Builder colorByRowLabel(final Boolean colorByRowLabel)
			{
				this.colorByRowLabel = colorByRowLabel;
				return this;
			}

			@Override
			public Builder groupByRowLabel(final Boolean groupByRowLabel)
			{
				this.groupByRowLabel = groupByRowLabel;
				return this;
			}

			@Override
			public Builder rowLabelStyle(final Styles rowLabelStyle)
			{
				this.rowLabelStyle = rowLabelStyle;
				return this;
			}

			@Override
			public Builder showRowLabels(final Boolean showRowLabels)
			{
				this.showRowLabels = showRowLabels;
				return this;
			}

			@Override
			public Builder singleColor(final String singleColor)
			{
				this.singleColor = singleColor;
				return this;
			}

			@Override
			public Timeline build()
			{
				return new Timeline.Default(this.barLabelStyle, this.colorByRowLabel, this.groupByRowLabel,
					this.rowLabelStyle, this.showRowLabels, this.singleColor);
			}
			
		}
		
	}
	
	public static class Default implements Timeline
	{
		private final Styles  barLabelStyle;
		private final Boolean colorByRowLabel;
		private final Boolean groupByRowLabel;
		private final Styles  rowLabelStyle;
		private final Boolean showRowLabels;
		private final String  singleColor;

		Default(
			final Styles barLabelStyle,
			final Boolean colorByRowLabel,
			final Boolean groupByRowLabel,
			final Styles rowLabelStyle,
			final Boolean showRowLabels,
			final String singleColor)
		{
			super();
			
			this.barLabelStyle   = barLabelStyle;
			this.colorByRowLabel = colorByRowLabel;
			this.groupByRowLabel = groupByRowLabel;
			this.rowLabelStyle   = rowLabelStyle;
			this.showRowLabels   = showRowLabels;
			this.singleColor     = singleColor;
		}
		
		@Override
		public Styles barLabelStyle()
		{
			return this.barLabelStyle;
		}

		@Override
		public Boolean colorByRowLabel()
		{
			return this.colorByRowLabel;
		}

		@Override
		public Boolean groupByRowLabel()
		{
			return this.groupByRowLabel;
		}

		@Override
		public Styles rowLabelStyle()
		{
			return this.rowLabelStyle;
		}

		@Override
		public Boolean showRowLabels()
		{
			return this.showRowLabels;
		}

		@Override
		public String singleColor()
		{
			return this.singleColor;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("barLabelStyle", this.barLabelStyle);
			obj.putIfNotNull("colorByRowLabel", this.colorByRowLabel);
			obj.putIfNotNull("groupByRowLabel", this.groupByRowLabel);
			obj.putIfNotNull("rowLabelStyle", this.rowLabelStyle);
			obj.putIfNotNull("showRowLabels", this.showRowLabels);
			obj.putIfNotNull("singleColor", this.singleColor);
			return obj.js();
		}
		
	}
	
}
