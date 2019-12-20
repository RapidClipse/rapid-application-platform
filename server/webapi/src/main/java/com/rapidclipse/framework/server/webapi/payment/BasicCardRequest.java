/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
