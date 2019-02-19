
package com.rapidclipse.framework.server.mobilekit;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;


/**
 * @author XDEV Software
 *
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Parameter
{
	public String name();
	
	public String defaultValue() default "";
}
