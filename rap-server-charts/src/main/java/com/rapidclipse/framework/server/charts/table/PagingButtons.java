/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.table;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface PagingButtons extends Serializable, JavaScriptable
{
	public static PagingButtons Count(final int count)
	{
		return () -> Json.create(count).toJson();
	}
	
	public static PagingButtons Both()
	{
		return () -> Json.create("both").toJson();
	}
	
	public static PagingButtons Prev()
	{
		return () -> Json.create("prev").toJson();
	}
	
	public static PagingButtons Next()
	{
		return () -> Json.create("next").toJson();
	}
	
	public static PagingButtons Auto()
	{
		return () -> Json.create("auto").toJson();
	}
}
