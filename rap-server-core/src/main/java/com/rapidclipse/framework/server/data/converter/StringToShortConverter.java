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
