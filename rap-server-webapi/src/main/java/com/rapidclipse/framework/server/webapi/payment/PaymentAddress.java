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
import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentAddress implements Serializable
{
	private String       city;
	private String       country;
	private String       dependentLocality;
	private String       organization;
	private String       phone;
	private String       postalCode;
	private String       recipient;
	private String       region;
	private String       sortingCode;
	private List<String> addressLine = new ArrayList<>();
	
	public PaymentAddress()
	{
	}
	
	public PaymentAddress(
		final String city,
		final String country,
		final String dependentLocality,
		final String organization,
		final String phone,
		final String postalCode,
		final String recipient,
		final String region,
		final String sortingCode,
		final List<String> addressLine)
	{
		this.city              = city;
		this.country           = country;
		this.dependentLocality = dependentLocality;
		this.organization      = organization;
		this.phone             = phone;
		this.postalCode        = postalCode;
		this.recipient         = recipient;
		this.region            = region;
		this.sortingCode       = sortingCode;
		this.addressLine       = addressLine;
	}
	
	public String getCity()
	{
		return this.city;
	}
	
	public PaymentAddress setCity(final String city)
	{
		this.city = city;
		return this;
	}
	
	public String getCountry()
	{
		return this.country;
	}
	
	public PaymentAddress setCountry(final String country)
	{
		this.country = country;
		return this;
	}
	
	public String getDependentLocality()
	{
		return this.dependentLocality;
	}
	
	public PaymentAddress setDependentLocality(final String dependentLocality)
	{
		this.dependentLocality = dependentLocality;
		return this;
	}
	
	public String getOrganization()
	{
		return this.organization;
	}
	
	public PaymentAddress setOrganization(final String organization)
	{
		this.organization = organization;
		return this;
	}
	
	public String getPhone()
	{
		return this.phone;
	}
	
	public PaymentAddress setPhone(final String phone)
	{
		this.phone = phone;
		return this;
	}
	
	public String getPostalCode()
	{
		return this.postalCode;
	}
	
	public PaymentAddress setPostalCode(final String postalCode)
	{
		this.postalCode = postalCode;
		return this;
	}
	
	public String getRecipient()
	{
		return this.recipient;
	}
	
	public PaymentAddress setRecipient(final String recipient)
	{
		this.recipient = recipient;
		return this;
	}
	
	public String getRegion()
	{
		return this.region;
	}
	
	public PaymentAddress setRegion(final String region)
	{
		this.region = region;
		return this;
	}
	
	public String getSortingCode()
	{
		return this.sortingCode;
	}
	
	public PaymentAddress setSortingCode(final String sortingCode)
	{
		this.sortingCode = sortingCode;
		return this;
	}
	
	public List<String> getAddressLine()
	{
		return this.addressLine;
	}
	
	public PaymentAddress setAddressLine(final List<String> addressLine)
	{
		this.addressLine = addressLine;
		return this;
	}
}
