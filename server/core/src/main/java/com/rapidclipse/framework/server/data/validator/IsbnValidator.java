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
