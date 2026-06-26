/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa.dal;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import jakarta.persistence.CacheRetrieveMode;
import jakarta.persistence.CacheStoreMode;


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
