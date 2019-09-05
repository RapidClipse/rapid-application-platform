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
