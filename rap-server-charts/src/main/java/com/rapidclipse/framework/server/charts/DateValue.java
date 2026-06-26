/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface DateValue extends Serializable, JavaScriptable
{
	public static DateValue Date(final LocalDate value)
	{
		requireNonNull(value);
		return () -> Static.js(value);
	}

	public static DateValue DateTime(final LocalDateTime value)
	{
		requireNonNull(value);
		return () -> Static.js(value);
	}

	public static DateValue Timestamp(final long timestamp)
	{
		return () -> Static.js(timestamp);
	}
}
