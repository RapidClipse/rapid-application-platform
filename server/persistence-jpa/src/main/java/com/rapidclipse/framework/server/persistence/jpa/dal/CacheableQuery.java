
package com.rapidclipse.framework.server.persistence.jpa.dal;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.persistence.CacheRetrieveMode;
import javax.persistence.CacheStoreMode;


/**
 * @author XDEV Software
 *
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface CacheableQuery
{
	public static enum Kind
	{
		FIND_ALL,
		REMOVE_BY_ID
	}
	
	boolean cache() default true;
	
	Kind kind();
	
	String region() default "";
	
	CacheStoreMode storeMode() default CacheStoreMode.USE;
	
	CacheRetrieveMode retrieveMode() default CacheRetrieveMode.USE;
}
