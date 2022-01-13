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
package com.rapidclipse.framework.server.data.validator;

import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
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
		
		public Pattern getPattern()
		{
			return this.pattern;
		}
	}
	
	private final static Pattern whiteSpacePattern = Pattern.compile("\\s");
	
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
				() -> MessageFormat.format(getResourceString(context, "passwordTooShort"), this.minLength)));
		}
		
		if(!isWhitespacesAllowed() && whiteSpacePattern.matcher(value).find())
		{
			return ValidationResult.error(getErrorMessage(value,
				() -> getResourceString(context, "noWhitespaceAllowed")));
		}
		
		if(this.minCompliedConditions > 0 && this.conditions != null)
		{
			int compliedConditions = 0;
			
			for(final Condition condition : this.conditions)
			{
				if(condition.getPattern().matcher(value).find())
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
					() -> {
						final String conditionsStr = Arrays.stream(this.conditions)
							.map(condition -> getResourceString(context, "Condition." + condition.name()))
							.collect(Collectors.joining(", "));
						return MessageFormat.format(getResourceString(context, "conditionsNotMet"),
							this.minCompliedConditions, conditionsStr);
					}));
			}
		}
		
		return ValidationResult.ok();
	}
	
	protected String getResourceString(final ValueContext context, final String name)
	{
		final Optional<Locale> optLocale = context.getLocale();
		return optLocale.isPresent()
			? StringResourceUtils.getResourceString(name, optLocale.get(), this)
			: StringResourceUtils.getResourceString(name, this);
	}
	
	protected String getErrorMessage(final String value, final Supplier<String> alternative)
	{
		final String message = getMessage(value);
		return !StringUtils.isEmpty(message)
			? message
			: alternative.get();
	}
}
