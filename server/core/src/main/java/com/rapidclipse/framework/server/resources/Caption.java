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
package com.rapidclipse.framework.server.resources;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * The caption annotation defines the {@link String} representation of an
 * element.
 * <p>
 * The element can either be a type or an attribute (field respectively method).
 * <p>
 * If a type is annotated with @Caption the {@link #value()} can contain member
 * variables which will be replaced by their actual values of the object
 * instance. The variable syntax is <code>{%memberName}</code>.
 * <p>
 * String resource tags - <code>{$resourceVariable}</code> - are supported too,
 * see
 * {@link StringResourceProvider#lookupResourceString(String, java.util.Locale, Object)
 * String resources}.
 * <p>
 * Example:
 *
 * <pre>
 * &#64;Caption("Person: {%lastName}, {%firstName}")
 * &#64;Entity
 * public class Person
 * {
 * 	private String firstName;
 * 	private String lastName;
 * 	private Date dob;
 *  ...
 * }
 *
 * Person p = new Person();
 * p.setFirstName("Peter");
 * p.setLastName("Smith");
 * String caption = CaptionUtils.resolveCaption(p); // -&gt; Person: Smith, Peter
 * </pre>
 *
 * The caption annotation is used e.g. by all UI components which contain
 * entites, such as combobox, list, table or filter. The table uses the
 * attributes' captions as column titles.
 *
 * @see CaptionUtils
 * @see CaptionResolver
 *
 *
 * @author XDEV Software
 *
 */
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Caption
{
	/**
	 * The caption's format string. Details are shown in the {@link Caption
	 * class doc}.
	 */
	public String value() default "";
}
