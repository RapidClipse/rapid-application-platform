
package com.rapidclipse.framework.server.webapi.payment;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class BasicCardResponse implements Serializable
{
	private String         cardNumber;
	private String         cardholderName;
	private String         cardSecurityCode;
	private String         expiryMonth;
	private String         expiryYear;
	private PaymentAddress billingAddress;
	
	public BasicCardResponse()
	{
	}
	
	public BasicCardResponse(
		final String cardNumber,
		final String cardholderName,
		final String cardSecurityCode,
		final String expiryMonth,
		final String expiryYear,
		final PaymentAddress billingAddress)
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
	
	public BasicCardResponse setCardNumber(final String cardNumber)
	{
		this.cardNumber = cardNumber;
		return this;
	}
	
	public String getCardholderName()
	{
		return this.cardholderName;
	}
	
	public BasicCardResponse setCardholderName(final String cardholderName)
	{
		this.cardholderName = cardholderName;
		return this;
	}
	
	public String getCardSecurityCode()
	{
		return this.cardSecurityCode;
	}
	
	public BasicCardResponse setCardSecurityCode(final String cardSecurityCode)
	{
		this.cardSecurityCode = cardSecurityCode;
		return this;
	}
	
	public String getExpiryMonth()
	{
		return this.expiryMonth;
	}
	
	public BasicCardResponse setExpiryMonth(final String expiryMonth)
	{
		this.expiryMonth = expiryMonth;
		return this;
	}
	
	public String getExpiryYear()
	{
		return this.expiryYear;
	}
	
	public BasicCardResponse setExpiryYear(final String expiryYear)
	{
		this.expiryYear = expiryYear;
		return this;
	}
	
	public PaymentAddress getBillingAddress()
	{
		return this.billingAddress;
	}
	
	public BasicCardResponse setBillingAddress(final PaymentAddress billingAddress)
	{
		this.billingAddress = billingAddress;
		return this;
	}
}
