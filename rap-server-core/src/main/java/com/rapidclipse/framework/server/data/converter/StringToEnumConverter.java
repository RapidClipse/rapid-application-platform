/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
