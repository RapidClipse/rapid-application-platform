/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.payment;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentAmount implements Serializable
{
	private String currency;
	private String value;

	public PaymentAmount()
	{
	}

	public PaymentAmount(final String currency, final String value)
	{
		this.currency = currency;
		this.value    = value;
	}

	public String getCurrency()
	{
		return this.currency;
	}

	public PaymentAmount setCurrency(final String currency)
	{
		this.currency = currency;
		return this;
	}

	public String getValue()
	{
		return this.value;
	}

	public PaymentAmount setValue(final String value)
	{
		this.value = value;
		return this;
	}
}
