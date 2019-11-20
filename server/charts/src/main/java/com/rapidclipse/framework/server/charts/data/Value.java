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

package com.rapidclipse.framework.server.charts.data;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalTime;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Value implements Serializable
{
	private Object valueObject; // string, number, date, datetime

	public Value(final Object v)
	{
		this.valueObject = v;
	}

	public Object getValueObject()
	{
		return this.valueObject;
	}

	public void setVAlueObject(final Object v)
	{
		this.valueObject = v;
	}
	
	@Override
	public String toString()
	{
		if(this.valueObject instanceof Number || this.valueObject == null)
		{
			return "" + this.valueObject;
		}
		else if(this.valueObject instanceof LocalDate)
		{
			final LocalDate date  = (LocalDate)this.valueObject;
			final int       year  = date.getYear();
			final int       month = date.getMonthValue() - 1;
			final int       day   = date.getDayOfMonth();
			
			return "new Date(" + year + "," + month + "," + day + ")";
		}
		else if(this.valueObject instanceof LocalTime)
		{
			final LocalTime time = (LocalTime)this.valueObject;
			return "new Date(0,0,0," + time.getHour() + "," + time.getMinute() + "," + time.getSecond() + ")";
		}
		else
		{
			return "'" + this.valueObject + "'";
		}
		
	}
}
