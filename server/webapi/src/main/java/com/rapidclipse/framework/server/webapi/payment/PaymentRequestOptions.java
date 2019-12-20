
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
