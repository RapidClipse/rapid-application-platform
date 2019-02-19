
package com.rapidclipse.framework.server.mobilekit;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;


/**
 * @author XDEV Software
 *
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Plugin
{
	public String name() default "";
	
	public String spec() default "";
	
	public PluginSource source() default PluginSource.NPM;
}
