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
