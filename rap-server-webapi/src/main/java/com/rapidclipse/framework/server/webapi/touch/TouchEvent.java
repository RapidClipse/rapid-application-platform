/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.touch;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class TouchEvent implements Serializable
{
	public static enum Type
	{
		TOUCH_START,
		TOUCH_END,
		TOUCH_MOVE,
		TOUCH_CANCEL
	}
	
	Type                            type;
	private final boolean           altKey;
	private final List<TouchObject> changedTouches = new ArrayList<>();
	private final boolean           ctrlKey;
	private final boolean           metaKey;
	private final boolean           shiftKey;
	private final List<TouchObject> targetTouches  = new ArrayList<>();
	private final List<TouchObject> touches        = new ArrayList<>();

	public TouchEvent(final boolean altKey, final boolean ctrlKey, final boolean metaKey, final boolean shiftKey)
	{
		this.altKey   = altKey;
		this.ctrlKey  = ctrlKey;
		this.metaKey  = metaKey;
		this.shiftKey = shiftKey;
	}

	public Type getType()
	{
		return this.type;
	}

	public boolean isAltKey()
	{
		return this.altKey;
	}

	public List<TouchObject> getChangedTouches()
	{
		return this.changedTouches;
	}

	public boolean isCtrlKey()
	{
		return this.ctrlKey;
	}

	public boolean isMetaKey()
	{
		return this.metaKey;
	}

	public boolean isShiftKey()
	{
		return this.shiftKey;
	}

	public List<TouchObject> getTargetTouches()
	{
		return this.targetTouches;
	}

	public List<TouchObject> getTouches()
	{
		return this.touches;
	}

	public static class TouchObject implements Serializable
	{
		private final long identifier;
		private final double screenX;
		private final double screenY;
		private final double clientX;
		private final double clientY;
		private final double pageX;
		private final double pageY;
		private final float  radiusX;
		private final float  radiusY;
		private final float  rotationAngle;
		private final float  force;

		public TouchObject(
			final long identifier,
			final double screenX,
			final double screenY,
			final double clientX,
			final double clientY,
			final double pageX,
			final double pageY,
			final float radiusX,
			final float radiusY,
			final float rotationAngle,
			final float force)
		{
			this.identifier    = identifier;
			this.screenX       = screenX;
			this.screenY       = screenY;
			this.clientX       = clientX;
			this.clientY       = clientY;
			this.pageX         = pageX;
			this.pageY         = pageY;
			this.radiusX       = radiusX;
			this.radiusY       = radiusY;
			this.rotationAngle = rotationAngle;
			this.force         = force;
		}

		public long getIdentifier()
		{
			return this.identifier;
		}

		public double getScreenX()
		{
			return this.screenX;
		}

		public double getScreenY()
		{
			return this.screenY;
		}

		public double getClientX()
		{
			return this.clientX;
		}

		public double getClientY()
		{
			return this.clientY;
		}

		public double getPageX()
		{
			return this.pageX;
		}

		public double getPageY()
		{
			return this.pageY;
		}

		public float getRadiusX()
		{
			return this.radiusX;
		}

		public float getRadiusY()
		{
			return this.radiusY;
		}

		public float getRotationAngle()
		{
			return this.rotationAngle;
		}

		public float getForce()
		{
			return this.force;
		}
	}
}
