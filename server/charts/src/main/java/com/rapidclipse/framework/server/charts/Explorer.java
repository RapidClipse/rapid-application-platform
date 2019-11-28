
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.EnumSet;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
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

	public Double maxZoomIn();

	public Double maxZoomOut();

	public Double zoomDelta();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder actions(EnumSet<Action> actions);

		public Builder axis(Axis axis);

		public Builder keepInBounds(Boolean keepInBounds);

		public Builder maxZoomIn(Double maxZoomIn);

		public Builder maxZoomOut(Double maxZoomOut);

		public Builder zoomDelta(Double zoomDelta);

		public Explorer build();

		public static class Default implements Builder
		{
			private EnumSet<Action> actions;
			private Axis            axis;
			private Boolean         keepInBounds;
			private Double          maxZoomIn;
			private Double          maxZoomOut;
			private Double          zoomDelta;

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
			public Builder maxZoomIn(final Double maxZoomIn)
			{
				this.maxZoomIn = maxZoomIn;
				return this;
			}

			@Override
			public Builder maxZoomOut(final Double maxZoomOut)
			{
				this.maxZoomOut = maxZoomOut;
				return this;
			}

			@Override
			public Builder zoomDelta(final Double zoomDelta)
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
		private final Double          maxZoomIn;
		private final Double          maxZoomOut;
		private final Double          zoomDelta;

		Default(
			final EnumSet<Action> actions,
			final Axis axis,
			final Boolean keepInBounds,
			final Double maxZoomIn,
			final Double maxZoomOut,
			final Double zoomDelta)
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
		public Double maxZoomIn()
		{
			return this.maxZoomIn;
		}

		@Override
		public Double maxZoomOut()
		{
			return this.maxZoomOut;
		}

		@Override
		public Double zoomDelta()
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