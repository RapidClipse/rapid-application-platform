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

import java.util.List;

import com.rapidclipse.framework.server.charts.pie.PieSlice;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
public class Slices
{
	private final List<PieSlice> part;
	
	public Slices(final List<PieSlice> part)
	{
		super();
		this.part = part;
		
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		int i = 0;
		for(final PieSlice s : this.part)
		{
			str.append(i + ": {");
			
			if(s.getColor() != null)
			{
				str.append("color: '" + s.getColor() + "', ");
			}
			str.append("offset: " + s.getOffset() + ", textStyle: "
				+ s.getTextStyle() + "},");
			i++;
		}
		str.delete(str.length() - 1, str.length());
		str.append("}");

		return str.toString();
	}
	
}
