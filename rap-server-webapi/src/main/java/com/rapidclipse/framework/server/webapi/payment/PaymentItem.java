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
public class PaymentItem implements Serializable
{
	private String        label;
	private PaymentAmount amount;

	public PaymentItem()
	{
	}

	public PaymentItem(final String label, final PaymentAmount amount)
	{
		this.label  = label;
		this.amount = amount;
	}

	public String getLabel()
	{
		return this.label;
	}

	public PaymentItem setLabel(final String label)
	{
		this.label = label;
		return this;
	}

	public PaymentAmount getAmount()
	{
		return this.amount;
	}

	public PaymentItem setAmount(final PaymentAmount amount)
	{
		this.amount = amount;
		return this;
	}
}
