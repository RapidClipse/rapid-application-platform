/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.validator;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * <p>
 * Email address validator.
 * </p>
 * <p>
 * Based on a script by <a href="mailto:stamhankar@hotmail.com">Sandeep V. Tamhankar</a>
 * http://javascript.internet.com
 * </p>
 *
 * @author XDEV Software
 *
 */
public class EmailValidator extends AbstractValidator<String>
{
	public EmailValidator(final String errorMessage)
	{
		super(errorMessage);
	}
	
	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		if(org.apache.commons.validator.routines.EmailValidator.getInstance().isValid(value))
		{
			return ValidationResult.ok();
		}

		return ValidationResult.error(getMessage(value));
	}
}
