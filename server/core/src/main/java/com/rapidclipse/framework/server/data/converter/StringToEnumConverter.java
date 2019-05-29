/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
