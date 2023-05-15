/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.charts.JavaScriptable;
import com.rapidclipse.framework.server.charts.TextStyle;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Node extends Serializable, JavaScriptable
{
	public TextStyle label();

	public Boolean interactivity();

	public Number labelPadding();

	public Number nodePadding();

	public Number width();

	public List<String> colors();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder label(TextStyle label);

		public Builder interactivity(Boolean interactivity);

		public Builder labelPadding(Number labelPadding);

		public Builder nodePadding(Number nodePadding);

		public Builder width(Number width);

		public Builder colors(List<String> colors);

		public Node build();

		public static class Default implements Builder
		{
			private TextStyle    label;
			private Boolean      interactivity;
			private Number       labelPadding;
			private Number       nodePadding;
			private Number       width;
			private List<String> colors;

			Default()
			{
				super();
			}

			@Override
			public Builder label(final TextStyle label)
			{
				this.label = label;
				return this;
			}

			@Override
			public Builder interactivity(final Boolean interactivity)
			{
				this.interactivity = interactivity;
				return this;
			}

			@Override
			public Builder labelPadding(final Number labelPadding)
			{
				this.labelPadding = labelPadding;
				return this;
			}

			@Override
			public Builder nodePadding(final Number nodePadding)
			{
				this.nodePadding = nodePadding;
				return this;
			}

			@Override
			public Builder width(final Number width)
			{
				this.width = width;
				return this;
			}

			@Override
			public Builder colors(final List<String> colors)
			{
				this.colors = colors;
				return this;
			}

			@Override
			public Node build()
			{
				return new Node.Default(this.label, this.interactivity, this.labelPadding, this.nodePadding, this.width,
					this.colors);
			}

		}

	}

	public static class Default implements Node
	{
		private final TextStyle    label;
		private final Boolean      interactivity;
		private final Number       labelPadding;
		private final Number       nodePadding;
		private final Number       width;
		private final List<String> colors;

		Default(
			final TextStyle label,
			final Boolean interactivity,
			final Number labelPadding,
			final Number nodePadding,
			final Number width,
			final List<String> colors)
		{
			super();
			
			this.label         = label;
			this.interactivity = interactivity;
			this.labelPadding  = labelPadding;
			this.nodePadding   = nodePadding;
			this.width         = width;
			this.colors        = colors;
		}

		@Override
		public TextStyle label()
		{
			return this.label;
		}

		@Override
		public Boolean interactivity()
		{
			return this.interactivity;
		}

		@Override
		public Number labelPadding()
		{
			return this.labelPadding;
		}

		@Override
		public Number nodePadding()
		{
			return this.nodePadding;
		}

		@Override
		public Number width()
		{
			return this.width;
		}

		@Override
		public List<String> colors()
		{
			return this.colors;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("label", this.label);
			obj.putIfNotNull("interactivity", this.interactivity);
			obj.putIfNotNull("labelPadding", this.labelPadding);
			obj.putIfNotNull("nodePadding", this.nodePadding);
			obj.putIfNotNull("width", this.width);
			obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
			return obj.js();
		}

	}

}
