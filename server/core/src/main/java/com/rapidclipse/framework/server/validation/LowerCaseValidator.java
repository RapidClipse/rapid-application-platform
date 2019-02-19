
package com.rapidclipse.framework.server.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.rapidclipse.framework.server.validation.constraints.LowerCase;


/**
 * @author XDEV Software
 *
 */

public class LowerCaseValidator implements ConstraintValidator<LowerCase, CharSequence>
{
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void initialize(final LowerCase constraintAnnotation)
	{
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isValid(final CharSequence value, final ConstraintValidatorContext context)
	{
		if(value == null)
		{
			return true;
		}

		final String string = value.toString();
		return string.toLowerCase().equals(string);
	}
}
