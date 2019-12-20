
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
