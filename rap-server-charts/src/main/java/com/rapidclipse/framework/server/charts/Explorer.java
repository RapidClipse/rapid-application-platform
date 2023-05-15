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
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.EnumSet;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Explorer extends Serializable, JavaScriptable
{
	public static enum Action implements JavaScriptable
	{
		DRAG_TO_PAN("dragToPan"),
		DRAG_TO_ZOOM("dragToZoom"),
		RIGHT_CLICK_TO_RESET("rightClickToReset");

		private final String js;

		private Action(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public static enum Axis implements JavaScriptable
	{
		HORIZONTAL("horizontal"),
		VERTICAL("vertical");

		private final String js;

		private Axis(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public EnumSet<Action> actions();

	public Axis axis();

	public Boolean keepInBounds();

	public Number maxZoomIn();

	public Number maxZoomOut();

	public Number zoomDelta();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder actions(EnumSet<Action> actions);

		public Builder axis(Axis axis);

		public Builder keepInBounds(Boolean keepInBounds);

		public Builder maxZoomIn(Number maxZoomIn);

		public Builder maxZoomOut(Number maxZoomOut);

		public Builder zoomDelta(Number zoomDelta);

		public Explorer build();

		public static class Default implements Builder
		{
			private EnumSet<Action> actions;
			private Axis            axis;
			private Boolean         keepInBounds;
			private Number          maxZoomIn;
			private Number          maxZoomOut;
			private Number          zoomDelta;

			Default()
			{
				super();
			}

			@Override
			public Builder actions(final EnumSet<Action> actions)
			{
				this.actions = actions;
				return this;
			}

			@Override
			public Builder axis(final Axis axis)
			{
				this.axis = axis;
				return this;
			}

			@Override
			public Builder keepInBounds(final Boolean keepInBounds)
			{
				this.keepInBounds = keepInBounds;
				return this;
			}

			@Override
			public Builder maxZoomIn(final Number maxZoomIn)
			{
				this.maxZoomIn = maxZoomIn;
				return this;
			}

			@Override
			public Builder maxZoomOut(final Number maxZoomOut)
			{
				this.maxZoomOut = maxZoomOut;
				return this;
			}

			@Override
			public Builder zoomDelta(final Number zoomDelta)
			{
				this.zoomDelta = zoomDelta;
				return this;
			}

			@Override
			public Explorer build()
			{
				return new Explorer.Default(this.actions, this.axis, this.keepInBounds, this.maxZoomIn, this.maxZoomOut,
					this.zoomDelta);
			}
		}
	}

	public static class Default implements Explorer
	{
		private final EnumSet<Action> actions;
		private final Axis            axis;
		private final Boolean         keepInBounds;
		private final Number          maxZoomIn;
		private final Number          maxZoomOut;
		private final Number          zoomDelta;

		Default(
			final EnumSet<Action> actions,
			final Axis axis,
			final Boolean keepInBounds,
			final Number maxZoomIn,
			final Number maxZoomOut,
			final Number zoomDelta)
		{
			super();

			this.actions      = actions;
			this.axis         = axis;
			this.keepInBounds = keepInBounds;
			this.maxZoomIn    = maxZoomIn;
			this.maxZoomOut   = maxZoomOut;
			this.zoomDelta    = zoomDelta;
		}

		@Override
		public EnumSet<Action> actions()
		{
			return this.actions;
		}

		@Override
		public Axis axis()
		{
			return this.axis;
		}

		@Override
		public Boolean keepInBounds()
		{
			return this.keepInBounds;
		}

		@Override
		public Number maxZoomIn()
		{
			return this.maxZoomIn;
		}

		@Override
		public Number maxZoomOut()
		{
			return this.maxZoomOut;
		}

		@Override
		public Number zoomDelta()
		{
			return this.zoomDelta;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("actions", new ArrayHelper().addAllScriptables(this.actions));
			obj.putIfNotNull("axis", this.axis);
			obj.putIfNotNull("keepInBounds", this.keepInBounds);
			obj.putIfNotNull("maxZoomIn", this.maxZoomIn);
			obj.putIfNotNull("maxZoomOut", this.maxZoomOut);
			obj.putIfNotNull("zoomDelta", this.zoomDelta);
			return obj.js();
		}
	}
}
