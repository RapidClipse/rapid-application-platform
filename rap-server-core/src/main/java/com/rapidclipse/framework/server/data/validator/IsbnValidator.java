/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
