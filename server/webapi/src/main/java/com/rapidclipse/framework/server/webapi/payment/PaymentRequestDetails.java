
package com.rapidclipse.framework.server.webapi.payment;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PaymentRequestDetails implements Serializable
{
	private PaymentItem                 total;
	private List<PaymentItem>           displayItems    = new ArrayList<>();
	private List<PaymentShippingOption> shippingOptions = new ArrayList<>();

	public PaymentRequestDetails()
	{
	}

	public PaymentRequestDetails(
		final PaymentItem total,
		final List<PaymentItem> displayItems,
		final List<PaymentShippingOption> shippingOptions)
	{
		this.total           = total;
		this.displayItems    = displayItems;
		this.shippingOptions = shippingOptions;
	}

	public PaymentItem getTotal()
	{
		return this.total;
	}

	public PaymentRequestDetails setTotal(final PaymentItem total)
	{
		this.total = total;
		return this;
	}

	public List<PaymentItem> getDisplayItems()
	{
		return this.displayItems;
	}

	public PaymentRequestDetails setDisplayItems(final List<PaymentItem> displayItems)
	{
		this.displayItems = displayItems;
		return this;
	}

	public List<PaymentShippingOption> getShippingOptions()
	{
		return this.shippingOptions;
	}

	public PaymentRequestDetails setShippingOptions(final List<PaymentShippingOption> shippingOptions)
	{
		this.shippingOptions = shippingOptions;
		return this;
	}
}
