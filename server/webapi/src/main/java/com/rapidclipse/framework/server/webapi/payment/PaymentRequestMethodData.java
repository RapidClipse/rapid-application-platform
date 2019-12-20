
package com.rapidclipse.framework.server.webapi.payment;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentRequestMethodData implements Serializable
{
	private String supportedMethods;
	// This can be also of different type depending on supportedMethods it seems
	private BasicCardRequest data;

	public PaymentRequestMethodData()
	{
	}

	public PaymentRequestMethodData(final String supportedMethods, final BasicCardRequest data)
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

	public BasicCardRequest getData()
	{
		return this.data;
	}

	public PaymentRequestMethodData setData(final BasicCardRequest data)
	{
		this.data = data;
		return this;
	}
}
