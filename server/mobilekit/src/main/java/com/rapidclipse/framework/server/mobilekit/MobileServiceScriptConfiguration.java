
package com.rapidclipse.framework.server.mobilekit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * @author XDEV Software
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface MobileServiceScriptConfiguration
{
	public Plugin[] plugins() default {};
	
	public Parameter[] params() default {};
}
