
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
