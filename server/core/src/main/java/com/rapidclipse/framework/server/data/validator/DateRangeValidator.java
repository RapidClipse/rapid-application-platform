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

package com.rapidclipse.framework.server.data.validator;

import java.util.Comparator;
import java.util.Date;

import com.vaadin.flow.data.validator.RangeValidator;


/**
 * Validator for validating that a {@link Date} is inside a given range.
 *
 * @author XDEV Software
 */
public class DateRangeValidator extends RangeValidator<Date>
{
	/**
	 * Creates a validator for checking that a Date is within a given
	 * range.
	 * <p>
	 * By default the range is inclusive i.e. both minValue and maxValue are
	 * valid values. Use {@link #setMinValueIncluded(boolean)} or
	 * {@link #setMaxValueIncluded(boolean)} to change it.
	 * </p>
	 *
	 * @param errorMessage
	 *            the message to display in case the value does not validate.
	 * @param minValue
	 *            The minimum value to accept or null for no limit
	 * @param maxValue
	 *            The maximum value to accept or null for no limit
	 */
	public DateRangeValidator(
		final String errorMessage,
		final Date minValue,
		final Date maxValue)
	{
		super(errorMessage, Comparator.naturalOrder(), minValue, maxValue);
	}
}
