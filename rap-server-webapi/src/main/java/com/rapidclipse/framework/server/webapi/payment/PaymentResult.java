/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.webapi.payment;

import java.io.Serializable;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentResult implements Serializable
{
	// Seems like this again could be something different as well
	private final BasicCardResponse details;
	private final String            methodName;
	private final String            payerEmail;
	private final String            payerName;
	private final String            payerPhone;
	private final String            requestId;
	private final PaymentAddress    shippingAddress;
	private final String            shippingOption;
	
	public PaymentResult(
		final BasicCardResponse details,
		final String methodName,
		final String payerEmail,
		final String payerName,
		final String payerPhone,
		final String requestId,
		final PaymentAddress shippingAddress,
		final String shippingOption)
	{
		this.details         = details;
		this.methodName      = methodName;
		this.payerEmail      = payerEmail;
		this.payerName       = payerName;
		this.payerPhone      = payerPhone;
		this.requestId       = requestId;
		this.shippingAddress = shippingAddress;
		this.shippingOption  = shippingOption;
	}
	
	public BasicCardResponse getDetails()
	{
		return this.details;
	}
	
	public String getMethodName()
	{
		return this.methodName;
	}
	
	public String getPayerEmail()
	{
		return this.payerEmail;
	}
	
	public String getPayerName()
	{
		return this.payerName;
	}
	
	public String getPayerPhone()
	{
		return this.payerPhone;
	}
	
	public String getRequestId()
	{
		return this.requestId;
	}
	
	public PaymentAddress getShippingAddress()
	{
		return this.shippingAddress;
	}
	
	public String getShippingOption()
	{
		return this.shippingOption;
	}
}
