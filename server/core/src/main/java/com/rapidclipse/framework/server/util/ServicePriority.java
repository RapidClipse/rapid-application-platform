
package com.rapidclipse.framework.server.util;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 *
 * @author XDEV Software
 *
 * @see ServiceLoader
 *
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface ServicePriority
{
	public final static int MIN     = Integer.MIN_VALUE;
	public final static int DEFAULT = 0;
	public final static int MAX     = Integer.MAX_VALUE;
	
	public int value() default DEFAULT;
}
