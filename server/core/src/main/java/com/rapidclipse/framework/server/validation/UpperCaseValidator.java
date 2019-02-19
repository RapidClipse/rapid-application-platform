
package com.rapidclipse.framework.server.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.rapidclipse.framework.server.validation.constraints.UpperCase;


/**
 * @author XDEV Software
 *
 */

public class UpperCaseValidator implements ConstraintValidator<UpperCase, CharSequence>
{
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void initialize(final UpperCase constraintAnnotation)
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
		return string.toUpperCase().equals(string);
	}
}
