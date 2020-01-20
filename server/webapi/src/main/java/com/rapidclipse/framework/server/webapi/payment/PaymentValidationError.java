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


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentValidationError implements Serializable
{
	private String          error;
	private PayerError      payer;
	private BasicCardErrors paymentMethod;
	private AddressErrors   shippingAddress;

	public PaymentValidationError()
	{
	}

	public PaymentValidationError(
		final String error,
		final PayerError payer,
		final BasicCardErrors paymentMethod,
		final AddressErrors shippingAddress)
	{
		this.error           = error;
		this.payer           = payer;
		this.paymentMethod   = paymentMethod;
		this.shippingAddress = shippingAddress;
	}

	public String getError()
	{
		return this.error;
	}

	public PaymentValidationError setError(final String error)
	{
		this.error = error;
		return this;
	}

	public PayerError getPayer()
	{
		return this.payer;
	}

	public PaymentValidationError setPayer(final PayerError payer)
	{
		this.payer = payer;
		return this;
	}

	public BasicCardErrors getPaymentMethod()
	{
		return this.paymentMethod;
	}

	public PaymentValidationError setPaymentMethod(final BasicCardErrors paymentMethod)
	{
		this.paymentMethod = paymentMethod;
		return this;
	}

	public AddressErrors getShippingAddress()
	{
		return this.shippingAddress;
	}

	public PaymentValidationError setShippingAddress(final AddressErrors shippingAddress)
	{
		this.shippingAddress = shippingAddress;
		return this;
	}

	public static class PayerError
	{
		private String email;
		private String name;
		private String phone;

		public PayerError()
		{
		}

		public PayerError(final String email, final String name, final String phone)
		{
			this.email = email;
			this.name  = name;
			this.phone = phone;
		}

		public String getEmail()
		{
			return this.email;
		}

		public PayerError setEmail(final String email)
		{
			this.email = email;
			return this;
		}

		public String getName()
		{
			return this.name;
		}

		public PayerError setName(final String name)
		{
			this.name = name;
			return this;
		}

		public String getPhone()
		{
			return this.phone;
		}

		public PayerError setPhone(final String phone)
		{
			this.phone = phone;
			return this;
		}
	}

	public static class BasicCardErrors
	{
		private String        cardNumber;
		private String        cardholderName;
		private String        cardSecurityCode;
		private String        expiryMonth;
		private String        expiryYear;
		private AddressErrors billingAddress;

		public BasicCardErrors()
		{
		}

		public BasicCardErrors(
			final String cardNumber,
			final String cardholderName,
			final String cardSecurityCode,
			final String expiryMonth,
			final String expiryYear,
			final AddressErrors billingAddress)
		{
			this.cardNumber       = cardNumber;
			this.cardholderName   = cardholderName;
			this.cardSecurityCode = cardSecurityCode;
			this.expiryMonth      = expiryMonth;
			this.expiryYear       = expiryYear;
			this.billingAddress   = billingAddress;
		}

		public String getCardNumber()
		{
			return this.cardNumber;
		}

		public BasicCardErrors setCardNumber(final String cardNumber)
		{
			this.cardNumber = cardNumber;
			return this;
		}

		public String getCardholderName()
		{
			return this.cardholderName;
		}

		public BasicCardErrors setCardholderName(final String cardholderName)
		{
			this.cardholderName = cardholderName;
			return this;
		}

		public String getCardSecurityCode()
		{
			return this.cardSecurityCode;
		}

		public BasicCardErrors setCardSecurityCode(final String cardSecurityCode)
		{
			this.cardSecurityCode = cardSecurityCode;
			return this;
		}

		public String getExpiryMonth()
		{
			return this.expiryMonth;
		}

		public BasicCardErrors setExpiryMonth(final String expiryMonth)
		{
			this.expiryMonth = expiryMonth;
			return this;
		}

		public String getExpiryYear()
		{
			return this.expiryYear;
		}

		public BasicCardErrors setExpiryYear(final String expiryYear)
		{
			this.expiryYear = expiryYear;
			return this;
		}

		public AddressErrors getBillingAddress()
		{
			return this.billingAddress;
		}

		public BasicCardErrors setBillingAddress(final AddressErrors billingAddress)
		{
			this.billingAddress = billingAddress;
			return this;
		}
	}

	public static class AddressErrors
	{
		private String addressLine;
		private String city;
		private String country;
		private String dependentLocality;
		private String organization;
		private String phone;
		private String postalCode;
		private String recipient;
		private String region;
		private String sortingCode;

		public AddressErrors()
		{
		}

		public AddressErrors(
			final String addressLine,
			final String city,
			final String country,
			final String dependentLocality,
			final String organization,
			final String phone,
			final String postalCode,
			final String recipient,
			final String region,
			final String sortingCode)
		{
			this.addressLine       = addressLine;
			this.city              = city;
			this.country           = country;
			this.dependentLocality = dependentLocality;
			this.organization      = organization;
			this.phone             = phone;
			this.postalCode        = postalCode;
			this.recipient         = recipient;
			this.region            = region;
			this.sortingCode       = sortingCode;
		}

		public String getAddressLine()
		{
			return this.addressLine;
		}

		public AddressErrors setAddressLine(final String addressLine)
		{
			this.addressLine = addressLine;
			return this;
		}

		public String getCity()
		{
			return this.city;
		}

		public AddressErrors setCity(final String city)
		{
			this.city = city;
			return this;
		}

		public String getCountry()
		{
			return this.country;
		}

		public AddressErrors setCountry(final String country)
		{
			this.country = country;
			return this;
		}

		public String getDependentLocality()
		{
			return this.dependentLocality;
		}

		public AddressErrors setDependentLocality(final String dependentLocality)
		{
			this.dependentLocality = dependentLocality;
			return this;
		}

		public String getOrganization()
		{
			return this.organization;
		}

		public AddressErrors setOrganization(final String organization)
		{
			this.organization = organization;
			return this;
		}

		public String getPhone()
		{
			return this.phone;
		}

		public AddressErrors setPhone(final String phone)
		{
			this.phone = phone;
			return this;
		}

		public String getPostalCode()
		{
			return this.postalCode;
		}

		public AddressErrors setPostalCode(final String postalCode)
		{
			this.postalCode = postalCode;
			return this;
		}

		public String getRecipient()
		{
			return this.recipient;
		}

		public AddressErrors setRecipient(final String recipient)
		{
			this.recipient = recipient;
			return this;
		}

		public String getRegion()
		{
			return this.region;
		}

		public AddressErrors setRegion(final String region)
		{
			this.region = region;
			return this;
		}

		public String getSortingCode()
		{
			return this.sortingCode;
		}

		public AddressErrors setSortingCode(final String sortingCode)
		{
			this.sortingCode = sortingCode;
			return this;
		}
	}
}
