/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.webapi.payment;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentRequestDetails implements Serializable
{
	private PaymentItem                 total;
	private List<PaymentItem>           displayItems    = new ArrayList<>();
	private List<PaymentShippingOption> shippingOptions = new ArrayList<>();

	public PaymentRequestDetails()
	{
	}

	public PaymentRequestDetails(
		final PaymentItem total,
		final List<PaymentItem> displayItems,
		final List<PaymentShippingOption> shippingOptions)
	{
		this.total           = total;
		this.displayItems    = displayItems;
		this.shippingOptions = shippingOptions;
	}

	public PaymentItem getTotal()
	{
		return this.total;
	}

	public PaymentRequestDetails setTotal(final PaymentItem total)
	{
		this.total = total;
		return this;
	}

	public List<PaymentItem> getDisplayItems()
	{
		return this.displayItems;
	}

	public PaymentRequestDetails setDisplayItems(final List<PaymentItem> displayItems)
	{
		this.displayItems = displayItems;
		return this;
	}

	public List<PaymentShippingOption> getShippingOptions()
	{
		return this.shippingOptions;
	}

	public PaymentRequestDetails setShippingOptions(final List<PaymentShippingOption> shippingOptions)
	{
		this.shippingOptions = shippingOptions;
		return this;
	}
}
