
package com.rapidclipse.framework.server.webapi.payment;

import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class BasicCardRequest
{
	private List<CardNetworkIdentifier> supportedNetworks = new ArrayList<>();

	public BasicCardRequest()
	{
	}

	public BasicCardRequest(final List<CardNetworkIdentifier> supportedNetworks)
	{
		this.supportedNetworks = supportedNetworks;
	}

	public List<CardNetworkIdentifier> getSupportedNetworks()
	{
		return this.supportedNetworks;
	}

	public BasicCardRequest setSupportedNetworks(final List<CardNetworkIdentifier> supportedNetworks)
	{
		this.supportedNetworks = supportedNetworks;
		return this;
	}
}
