/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.data.validator;

import org.apache.commons.validator.routines.ISBNValidator;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * @author XDEV Software
 *
 */
public class IsbnValidator extends AbstractValidator<String>
{
	private final static ISBNValidator VALIDATOR = new ISBNValidator(false);

	public static enum CheckDigit
	{
		ISBN_10()
		{
			@Override
			boolean validate(final String value)
			{
				return VALIDATOR.isValidISBN10(value);
			}
		},
		ISBN_13()
		{
			@Override
			boolean validate(final String value)
			{
				return VALIDATOR.isValidISBN13(value);
			}
		};

		abstract boolean validate(String value);
	}

	private final CheckDigit[] checkDigits;
	
	public IsbnValidator(final String errorMessage)
	{
		this(errorMessage, CheckDigit.values());
	}

	public IsbnValidator(final String errorMessage, final CheckDigit... checkDigits)
	{
		super(errorMessage);

		this.checkDigits = checkDigits;
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		for(final CheckDigit checkDigit : this.checkDigits)
		{
			if(checkDigit.validate(value))
			{
				return ValidationResult.ok();
			}
		}

		return ValidationResult.error(getMessage(value));
	}
}
