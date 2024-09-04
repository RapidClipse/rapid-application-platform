/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.validator;

import java.net.MalformedURLException;
import java.net.URL;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 *
 * @author XDEV Software
 *
 */
public class UrlValidator extends AbstractValidator<String>
{
	public UrlValidator(final String errorMessage)
	{
		super(errorMessage);
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		try
		{
			new URL(value);

			return ValidationResult.ok();
		}
		catch(final MalformedURLException e)
		{
			return ValidationResult.error(getMessage(value));
		}
	}
}
