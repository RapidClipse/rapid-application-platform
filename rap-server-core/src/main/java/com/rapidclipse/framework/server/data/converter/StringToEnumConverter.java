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
package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import com.vaadin.flow.data.binder.ErrorMessageProvider;
import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public class StringToEnumConverter<E extends Enum<E>> implements Converter<String, E>
{
	private final Class<E>             enumType;
	private final E                    emptyValue;
	private final ErrorMessageProvider errorMessageProvider;

	public StringToEnumConverter(
		final Class<E> enumType,
		final E emptyValue,
		final ErrorMessageProvider errorMessageProvider)
	{
		super();

		this.enumType             = requireNonNull(enumType);
		this.emptyValue           = emptyValue;
		this.errorMessageProvider = requireNonNull(errorMessageProvider);
	}

	public StringToEnumConverter(final Class<E> enumType, final E emptyValue, final String errorMessage)
	{
		this(enumType, emptyValue, ctx -> errorMessage);
	}

	@Override
	public Result<E> convertToModel(String value, final ValueContext context)
	{
		if(value == null)
		{
			return Result.ok(null);
		}

		value = value.trim();

		if(value.length() == 0)
		{
			return Result.ok(this.emptyValue);
		}

		try
		{
			return Result.ok(Enum.valueOf(this.enumType, value));
		}
		catch(final IllegalArgumentException e)
		{
			return Result.error(this.errorMessageProvider.apply(context));
		}
	}
	
	@Override
	public String convertToPresentation(final E value, final ValueContext context)
	{
		return value == null ? null : value.name();
	}
}
