/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.validator;

import static java.util.Objects.requireNonNull;

import java.util.function.Supplier;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * @author XDEV Software
 *
 */
public class PasswordMatchValidator extends AbstractValidator<String>
{
	private final Supplier<String> secondPassword;

	public PasswordMatchValidator(final Supplier<String> secondPassword)
	{
		this("Passwords don't match", secondPassword);
	}

	public PasswordMatchValidator(final String errorMessage, final Supplier<String> secondPassword)
	{
		super(errorMessage);

		this.secondPassword = requireNonNull(secondPassword);
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		final String secondPassword = this.secondPassword.get();

		final String pw1 = value != null ? value : "";
		final String pw2 = secondPassword != null ? secondPassword : "";

		return pw1.equals(pw2)
			? ValidationResult.ok()
			: ValidationResult.error(getMessage(value));
	}
}
