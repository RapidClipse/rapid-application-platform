
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
