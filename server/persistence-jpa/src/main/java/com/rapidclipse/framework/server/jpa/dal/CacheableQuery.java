/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa.dal;

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
