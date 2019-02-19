
package com.rapidclipse.framework.server.data.validator;

import java.util.Arrays;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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

	private final static Pattern WHITESPACE_PATTERN = Pattern.compile("\\s");

	private final int            minLength;
	private final boolean        whitespacesAllowed;
	private final int            minCompliedConditions;
	private final Condition[]    conditions;
	
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
			return ValidationResult
				.error("Password must have at least " + this.minLength + " characters.");
		}

		if(!isWhitespacesAllowed())
		{
			if(WHITESPACE_PATTERN.matcher(value).find())
			{
				return ValidationResult.error("No whitespaces allowed in password.");
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
				return ValidationResult
					.error("Password must have at least " + this.minCompliedConditions
						+ " of these: " + Arrays.stream(this.conditions).map(c -> c.name())
							.collect(Collectors.joining(", ")));
			}
		}

		return ValidationResult.ok();
	}
}
