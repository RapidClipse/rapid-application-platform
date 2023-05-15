/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentRequestOptions implements Serializable
{
	private boolean      requestPayerName;
	private boolean      requestPayerEmail;
	private boolean      requestPayerPhone;
	private boolean      requestShipping;
	private ShippingType shippingType = ShippingType.shipping;

	public PaymentRequestOptions()
	{
	}

	public PaymentRequestOptions(
		final boolean requestPayerName,
		final boolean requestPayerEmail,
		final boolean requestPayerPhone,
		final boolean requestShipping,
		final ShippingType shippingType)
	{
		this.requestPayerName  = requestPayerName;
		this.requestPayerEmail = requestPayerEmail;
		this.requestPayerPhone = requestPayerPhone;
		this.requestShipping   = requestShipping;
		this.shippingType      = shippingType;
	}

	public boolean isRequestPayerName()
	{
		return this.requestPayerName;
	}

	public PaymentRequestOptions setRequestPayerName(final boolean requestPayerName)
	{
		this.requestPayerName = requestPayerName;
		return this;
	}

	public boolean isRequestPayerEmail()
	{
		return this.requestPayerEmail;
	}

	public PaymentRequestOptions setRequestPayerEmail(final boolean requestPayerEmail)
	{
		this.requestPayerEmail = requestPayerEmail;
		return this;
	}

	public boolean isRequestPayerPhone()
	{
		return this.requestPayerPhone;
	}

	public PaymentRequestOptions setRequestPayerPhone(final boolean requestPayerPhone)
	{
		this.requestPayerPhone = requestPayerPhone;
		return this;
	}

	public boolean isRequestShipping()
	{
		return this.requestShipping;
	}

	public PaymentRequestOptions setRequestShipping(final boolean requestShipping)
	{
		this.requestShipping = requestShipping;
		return this;
	}

	public ShippingType getShippingType()
	{
		return this.shippingType;
	}

	public PaymentRequestOptions setShippingType(final ShippingType shippingType)
	{
		this.shippingType = shippingType;
		return this;
	}

	public enum ShippingType
	{
		shipping,
		delivery,
		pickup
	}
}
