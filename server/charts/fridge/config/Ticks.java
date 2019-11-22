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
 * TODO naming?
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Ticks
{
	private Object v;
	private String f;
	
	public Ticks()
	{
		super();
	}

	public Ticks(final Object v, final String f)
	{
		this.v = v;
		this.f = f;
	}
	
	public Object getV()
	{
		return this.v;
	}
	
	public void setV(final Object v)
	{
		this.v = v;
	}
	
	public String getF()
	{
		return this.f;
	}
	
	public void setF(final String f)
	{
		this.f = f;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{v: ");
		if(this.v instanceof Number)
		{
			str.append(this.v);
		}
		else
		{
			str.append("'" + this.v + "'");
		}
		str.append(", f:'" + this.f + "'}");
		
		return str.toString();
	}
}
