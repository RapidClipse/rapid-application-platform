/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
