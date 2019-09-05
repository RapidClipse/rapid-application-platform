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

import java.util.Arrays;
import java.util.ResourceBundle;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * @author XDEV Software
 */
public class PasswordValidator extends AbstractValidator<String>
{
	public static enum Condition
	{
		UPPERCASE_LETTERS("[A-Z]"),
		LOWERCASE_LETTERS("[a-z]"),
		NUMBERS("\\d"),
		SPECIAL_CHARACTERS("[^\\w\\s]");

		private Pattern pattern;

		private Condition(final String regex)
		{
			this.pattern = Pattern.compile(regex);
		}
	}

	private final static Pattern        whiteSpacePattern = Pattern.compile("\\s");
	private final static ResourceBundle resourceBundle    = ResourceBundle.getBundle(PasswordValidator.class.getName());

	private final int         minLength;
	private final boolean     whitespacesAllowed;
	private final int         minCompliedConditions;
	private final Condition[] conditions;

	public PasswordValidator(
		final int minLength,
		final boolean whitespacesAllowed,
		final int minCompliedConditions,
		final Condition... conditions)
	{
		this("", minLength, whitespacesAllowed, minCompliedConditions, conditions);
	}

	public PasswordValidator(
		final String errorMessage,
		final int minLength,
		final boolean whitespacesAllowed,
		final int minCompliedConditions,
		final Condition... conditions)
	{
		super(errorMessage);

		this.minLength             = minLength;
		this.whitespacesAllowed    = whitespacesAllowed;
		this.minCompliedConditions = minCompliedConditions;
		this.conditions            = conditions;
	}

	public int getMinLength()
	{
		return this.minLength;
	}

	public boolean isWhitespacesAllowed()
	{
		return this.whitespacesAllowed;
	}

	public Condition[] getConditions()
	{
		return this.conditions;
	}

	public int getMinCompliedConditions()
	{
		return this.minCompliedConditions;
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		if(this.minLength > 0 && value.length() < this.minLength)
		{
			return ValidationResult.error(getErrorMessage(value,
				() -> "Password must have at least " + this.minLength + " characters."));
		}

		if(!isWhitespacesAllowed())
		{
			if(whiteSpacePattern.matcher(value).find())
			{
				return ValidationResult.error(getErrorMessage(value,
					() -> "No whitespaces allowed in password."));
			}
		}

		if(this.minCompliedConditions > 0 && this.conditions != null)
		{
			int compliedConditions = 0;

			for(final Condition condition : this.conditions)
			{
				if(condition.pattern.matcher(value).find())
				{
					compliedConditions++;
					if(compliedConditions == this.minCompliedConditions)
					{
						// no more checks necessary
						break;
					}
				}
			}

			if(compliedConditions < this.minCompliedConditions)
			{
				return ValidationResult.error(getErrorMessage(value,
					() -> "The password must meet at least " + this.minCompliedConditions + " of these criteria:" +
						Arrays.stream(this.conditions)
							.map(this::getCaption)
							.collect(Collectors.joining(", "))));
			}
		}

		return ValidationResult.ok();
	}

	protected String getErrorMessage(final String value, final Supplier<String> alternative)
	{
		final String message = getMessage(value);
		return !StringUtils.isEmpty(message)
			? message
			: alternative.get();
	}

	protected String getCaption(final Condition condition)
	{
		return resourceBundle.getString(condition.name());
	}
}
