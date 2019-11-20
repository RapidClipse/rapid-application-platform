/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts.config;

/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class Ticks
{
	private Object v;
	private String f;

	public Ticks()
	{

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
