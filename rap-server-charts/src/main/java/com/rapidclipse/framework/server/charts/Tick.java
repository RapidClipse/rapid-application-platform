/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Objects;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Tick extends Serializable, JavaScriptable
{
	public Object value();

	public String formattedValue();

	public static Tick New(final Object value)
	{
		return new Default(value, null);
	}

	public static Tick New(final Object value, final String formattedValue)
	{
		return new Default(value, formattedValue);
	}

	public static class Default implements Tick
	{
		private final Object value;
		private final String formattedValue;

		Default(final Object value, final String formattedValue)
		{
			super();

			this.value          = Static.validateValue(Objects.requireNonNull(value));
			this.formattedValue = formattedValue;
		}

		@Override
		public Object value()
		{
			return this.value;
		}

		@Override
		public String formattedValue()
		{
			return this.formattedValue;
		}

		@Override
		public String js()
		{
			if(this.formattedValue == null)
			{
				return Static.js(this.value);
			}

			final ObjectHelper obj = new ObjectHelper();
			obj.putJson("v", Static.js(this.value));
			obj.putIfNotNull("f", this.formattedValue);
			return obj.js();
		}

	}

}
