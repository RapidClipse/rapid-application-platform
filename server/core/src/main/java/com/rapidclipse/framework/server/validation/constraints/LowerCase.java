
package com.rapidclipse.framework.server.validation.constraints;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

import com.rapidclipse.framework.server.validation.LowerCaseValidator;


/**
 * @author XDEV Software
 *
 */

@Target({METHOD, FIELD, ANNOTATION_TYPE})
@Retention(RUNTIME)
@Constraint(validatedBy = LowerCaseValidator.class)
@Documented
public @interface LowerCase
{
	String message() default "{rap.validation.constraints.lowercase}";
	
	Class<?>[] groups() default {};
	
	Class<? extends Payload>[] payload() default {};
	
	@Target({java.lang.annotation.ElementType.TYPE})
	@Retention(RetentionPolicy.RUNTIME)
	@Documented
	public static @interface List
	{
		public abstract LowerCase[] value();
	}
}
