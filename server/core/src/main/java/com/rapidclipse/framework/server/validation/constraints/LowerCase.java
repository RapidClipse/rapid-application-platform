/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
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
