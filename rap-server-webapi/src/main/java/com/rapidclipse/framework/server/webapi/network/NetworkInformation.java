/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.network;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class NetworkInformation implements Serializable
{
	private final double      downlink;
	private final double      downlinkMax;
	private final String      effectiveType;
	private final int         rtt;
	private final boolean     saveData;
	private final NetworkType type;
	
	public NetworkInformation(
		final double downlink,
		final double downlinkMax,
		final String effectiveType,
		final int rtt,
		final boolean saveData,
		final NetworkType type)
	{
		this.downlink      = downlink;
		this.downlinkMax   = downlinkMax;
		this.effectiveType = effectiveType;
		this.rtt           = rtt;
		this.saveData      = saveData;
		this.type          = type;
	}
	
	public double getDownlink()
	{
		return this.downlink;
	}
	
	public double getDownlinkMax()
	{
		return this.downlinkMax;
	}
	
	public String getEffectiveType()
	{
		return this.effectiveType;
	}
	
	/**
	 * This value is always a multiple of 25.
	 */
	public int getRtt()
	{
		return this.rtt;
	}
	
	public boolean isSaveData()
	{
		return this.saveData;
	}
	
	public NetworkType getType()
	{
		return this.type;
	}
	
	/* Not an enum as enum values cannot start with a number and also cannot contain dashes. */
	public static final String EFFECTIVE_TYPE_SLOW_2G = "slow-2g";
	public static final String EFFECTIVE_TYPE_2G      = "2g";
	public static final String EFFECTIVE_TYPE_3G      = "3g";
	public static final String EFFECTIVE_TYPE_4G      = "4g";
	
	public enum NetworkType
	{
		bluetooth,
		cellular,
		ethernet,
		none,
		wifi,
		wimax,
		other,
		unkown
	}
}
