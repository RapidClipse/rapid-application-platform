/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface LinkColor extends Serializable, JavaScriptable
{
	public String stroke();
	
	public Number strokeWidth();
	
	public String fill();
	
	public Number fillOpacity();
	
	public static LinkColor
		New(final String stroke, final Number strokeWidth, final String fill, final Number fillOpacity)
	{
		return new Default(stroke, strokeWidth, fill, fillOpacity);
	}
	
	public static class Default implements LinkColor
	{
		private final String stroke;
		private final Number strokeWidth;
		private final String fill;
		private final Number fillOpacity;
		
		Default(final String stroke, final Number strokeWidth, final String fill, final Number fillOpacity)
		{
			super();
			
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.fill        = fill;
			this.fillOpacity = fillOpacity;
		}
		
		@Override
		public String stroke()
		{
			return this.stroke;
		}
		
		@Override
		public Number strokeWidth()
		{
			return this.strokeWidth;
		}
		
		@Override
		public String fill()
		{
			return this.fill;
		}
		
		@Override
		public Number fillOpacity()
		{
			return this.fillOpacity;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("fill", this.fill);
			obj.putIfNotNull("fillOpacity", this.fillOpacity);
			return obj.js();
		}
		
	}
	
}
