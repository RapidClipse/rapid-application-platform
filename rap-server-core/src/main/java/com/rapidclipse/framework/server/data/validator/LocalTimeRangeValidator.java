/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.validator;

import java.time.LocalTime;
import java.util.Comparator;

import com.vaadin.flow.data.validator.RangeValidator;


/**
 * Validator for validating that a {@link LocalTime} is inside a given range.
 *
 * @author XDEV Software
 */
public class LocalTimeRangeValidator extends RangeValidator<LocalTime>
{
	/**
	 * Creates a validator for checking that a LocalTime is within a given
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
	public LocalTimeRangeValidator(
		final String errorMessage,
		final LocalTime minValue,
		final LocalTime maxValue)
	{
		super(errorMessage, Comparator.naturalOrder(), minValue, maxValue);
	}
}
