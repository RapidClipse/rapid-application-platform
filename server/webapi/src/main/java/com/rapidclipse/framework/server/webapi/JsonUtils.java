/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server.webapi;

import com.google.gson.Gson;

import elemental.json.JsonValue;
import elemental.json.impl.JreJsonFactory;


/**
 * Utility class that contains various convinience methods for working with vaadin's json values
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public final class JsonUtils
{
	public static final Gson GSON = new Gson();
	
	/**
	 * Convert an object into a {@link JsonValue}.
	 *
	 * @param obj
	 *            The object to be converted.
	 * @return The converted object.
	 */
	public static JsonValue encodeObject(final Object obj)
	{
		return new JreJsonFactory().parse(JsonUtils.GSON.toJson(obj));
	}
	
	private JsonUtils()
	{
		throw new Error();
	}
}
