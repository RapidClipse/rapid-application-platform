/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.config;

/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Options
{
	private Options()
	{
		// Private Constructor for Utility class
	}
	
	/***************************************************************************
	 *
	 * SIZE
	 *
	 **************************************************************************/
	public static final String SIZE_SMALL  = "small";
	public static final String SIZE_MEDIUM = "medium";
	public static final String SIZE_LARGE  = "large";
	
	/***************************************************************************
	 *
	 * POSITION
	 *
	 **************************************************************************/
	public static final String POSITION_BOTTOM  = "bottom";
	public static final String POSITION_TOP     = "top";
	public static final String POSITION_LABELED = "labeled";
	public static final String POSITION_LEFT    = "left";
	public static final String POSITION_NONE    = "none";
	public static final String POSITION_RIGHT   = "right";
	
	/***************************************************************************
	 *
	 * ALIGNMENT
	 *
	 **************************************************************************/
	public static final String ALIGNMENT_START  = "start";
	public static final String ALIGNMENT_CENTER = "center";
	public static final String ALIGNMENT_END    = "end";
	
	/***************************************************************************
	 *
	 * CURVETYPE
	 *
	 **************************************************************************/
	public static final String CURVETYPE_NONE     = "none";
	public static final String CURVETYPE_FUNCTION = "function";
	
	/***************************************************************************
	 *
	 * TEXTPOSITION
	 *
	 **************************************************************************/
	public static final String TEXTPOSITION_NONE = "none";
	public static final String TEXTPOSITION_OUT  = "out";
	public static final String TEXTPOSITION_IN   = "in";

	/***************************************************************************
	 *
	 * ORIENTATION
	 *
	 **************************************************************************/
	public static final String ORIENTATION_VERTICAL   = "vertical";
	public static final String ORIENTATION_HORIZONTAL = "horizontal";
	
	/***************************************************************************
	 *
	 * POINTSHAPE
	 *
	 **************************************************************************/
	public static final String POINTSHAPE_CIRCLE   = "circle";
	public static final String POINTSHAPE_TRIANGLE = "triangle";
	public static final String POINTSHAPE_SQUARE   = "square";
	public static final String POINTSHAPE_DIAMOND  = "diamond";
	public static final String POINTSHAPE_STAR     = "star";
	public static final String POINTSHAPE_POLYGON  = "polygon";
}
