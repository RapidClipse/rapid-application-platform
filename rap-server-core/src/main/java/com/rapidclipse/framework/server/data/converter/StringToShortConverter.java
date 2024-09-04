/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.converter;

import java.text.NumberFormat;
import java.util.Locale;

import com.vaadin.flow.data.binder.ErrorMessageProvider;
import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.AbstractStringToNumberConverter;


/**
 * @author XDEV Software
 *
 */
public class StringToShortConverter extends AbstractStringToNumberConverter<Short>
{
	public StringToShortConverter(
		final Short emptyValue,
		final ErrorMessageProvider errorMessageProvider)
	{
		super(emptyValue, errorMessageProvider);
	}
	
	public StringToShortConverter(final Short emptyValue, final String errorMessage)
	{
		super(emptyValue, errorMessage);
	}
	
	@Override
	protected NumberFormat getFormat(Locale locale)
	{
		if(locale == null)
		{
			locale = Locale.getDefault();
		}
		return NumberFormat.getIntegerInstance(locale);
	}
	
	@Override
	public Result<Short> convertToModel(final String value, final ValueContext context)
	{
		final Result<Number> n = convertToNumber(value, context);
		return n.flatMap(number -> {
			if(number == null)
			{
				return Result.ok(null);
			}
			else
			{
				final short shortValue = number.shortValue();
				if(shortValue == number.longValue())
				{
					return Result.ok(shortValue);
				}
				else
				{
					return Result.error(getErrorMessage(context));
				}
			}
		});
	}
}
