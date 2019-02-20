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
