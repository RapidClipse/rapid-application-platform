/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.screen;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class ScreenOrientation implements Serializable
{
	// The "type" field here is of type string, so that json parsing works without problems
	private String type;
	private int    angle;
	
	public ScreenOrientation(final OrientationType type, final int angle)
	{
		this.type  = type.toString();
		this.angle = angle;
	}
	
	public OrientationType getType()
	{
		return OrientationType.fromString(this.type);
	}
	
	public ScreenOrientation setType(final OrientationType type)
	{
		this.type = type.toString();
		return this;
	}
	
	public int getAngle()
	{
		return this.angle;
	}
	
	public ScreenOrientation setAngle(final int angle)
	{
		this.angle = angle;
		return this;
	}
	
	public enum OrientationType
	{
		portrait_primary,
		portrait_secondary,
		landscape_primary,
		landscape_secondary;
		
		/*
		 * These methods are for easy use to convert to and parse from JavaScript's ScreenOrientation.type
		 */
		
		@Override
		public String toString()
		{
			return this.name().replace('_', '-');
		}
		
		public static OrientationType fromString(final String str)
		{
			return OrientationType.valueOf(str.replace('-', '_'));
		}
	}
}
