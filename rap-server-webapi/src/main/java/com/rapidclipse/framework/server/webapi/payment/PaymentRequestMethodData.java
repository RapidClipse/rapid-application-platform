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

import elemental.json.JsonObject;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentRequestMethodData implements Serializable
{
	private String supportedMethods;
	private JsonObject data;

	public PaymentRequestMethodData()
	{
	}

	public PaymentRequestMethodData(final String supportedMethods, final JsonObject data)
	{
		this.supportedMethods = supportedMethods;
		this.data             = data;
	}

	public String getSupportedMethods()
	{
		return this.supportedMethods;
	}

	public PaymentRequestMethodData setSupportedMethods(final String supportedMethods)
	{
		this.supportedMethods = supportedMethods;
		return this;
	}

	public JsonObject getData()
	{
		return this.data;
	}

	public PaymentRequestMethodData setData(final JsonObject data)
	{
		this.data = data;
		return this;
	}
}
