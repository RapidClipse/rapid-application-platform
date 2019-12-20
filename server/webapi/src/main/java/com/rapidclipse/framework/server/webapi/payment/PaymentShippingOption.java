/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentShippingOption implements Serializable
{
	private String        id;
	private String        label;
	private PaymentAmount amount;
	private boolean       selected;

	public PaymentShippingOption()
	{
	}

	public PaymentShippingOption(
		final String id,
		final String label,
		final PaymentAmount amount,
		final boolean selected)
	{
		this.id       = id;
		this.label    = label;
		this.amount   = amount;
		this.selected = selected;
	}

	public String getId()
	{
		return this.id;
	}

	public PaymentShippingOption setId(final String id)
	{
		this.id = id;
		return this;
	}

	public String getLabel()
	{
		return this.label;
	}

	public PaymentShippingOption setLabel(final String label)
	{
		this.label = label;
		return this;
	}

	public PaymentAmount getAmount()
	{
		return this.amount;
	}

	public PaymentShippingOption setAmount(final PaymentAmount amount)
	{
		this.amount = amount;
		return this;
	}

	public boolean isSelected()
	{
		return this.selected;
	}

	public PaymentShippingOption setSelected(final boolean selected)
	{
		this.selected = selected;
		return this;
	}
}
