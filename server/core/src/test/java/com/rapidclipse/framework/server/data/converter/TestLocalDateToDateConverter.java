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

package com.rapidclipse.framework.server.data.converter;

import static org.junit.jupiter.api.Assertions.assertFalse;

import java.time.LocalDate;

import org.junit.jupiter.api.Test;

import com.vaadin.flow.data.binder.ValueContext;


/**
 * @author XDEV Software
 *
 */
public class TestLocalDateToDateConverter
{
	private final LocalDate    value        = LocalDate.now();
	private final ValueContext valueContext = new ValueContext();
	
	@Test
	void testLocalDateToUtilDate()
	{
		assertFalse(LocalDateToDateConverter.UtilDate().convertToModel(this.value, this.valueContext).isError());
	}
	
	@Test
	void testLocalDateToSqlDate()
	{
		assertFalse(LocalDateToDateConverter.SqlDate().convertToModel(this.value, this.valueContext).isError());
	}
	
	@Test
	void testLocalDateToLocalSqlTime()
	{
		assertFalse(LocalDateToDateConverter.SqlTime().convertToModel(this.value, this.valueContext).isError());
	}
	
	@Test
	void testLocalDateToOffsetSqlTimestamp()
	{
		assertFalse(LocalDateToDateConverter.SqlTimestamp().convertToModel(this.value, this.valueContext).isError());
	}
}
