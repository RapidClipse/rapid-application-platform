/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.webapi.notification;

import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class LocalNotificationOptions
{
	private final List<Action> actions = new ArrayList<>();
	// Image Url
	private String badge;
	private String body;
	// Text direction
	private Direction dir;
	// Image Url
	private String image;
	private String icon;
	// BCP 47 language tag
	private String  lang;
	private boolean renotify;
	private boolean requireInteraction;
	private boolean silent;
	// Needs to be set for renotify to work
	private String tag;
	private long   timestamp;
	private int[]  vibrate;

	public LocalNotificationOptions()
	{
		super();
	}
	
	public String getBadge()
	{
		return this.badge;
	}
	
	public LocalNotificationOptions setBadge(final String badge)
	{
		this.badge = badge;
		return this;
	}
	
	public String getBody()
	{
		return this.body;
	}
	
	public LocalNotificationOptions setBody(final String body)
	{
		this.body = body;
		return this;
	}
	
	public Direction getDir()
	{
		return this.dir;
	}
	
	public LocalNotificationOptions setDir(final Direction dir)
	{
		this.dir = dir;
		return this;
	}
	
	public String getImage()
	{
		return this.image;
	}
	
	public LocalNotificationOptions setImage(final String image)
	{
		this.image = image;
		return this;
	}

	public String getIcon()
	{
		return this.icon;
	}

	public LocalNotificationOptions setIcon(final String icon)
	{
		this.icon = icon;
		return this;
	}
	
	public String getLang()
	{
		return this.lang;
	}
	
	public LocalNotificationOptions setLang(final String lang)
	{
		this.lang = lang;
		return this;
	}
	
	public boolean isRenotify()
	{
		return this.renotify;
	}
	
	public LocalNotificationOptions setRenotify(final boolean renotify)
	{
		this.renotify = renotify;
		return this;
	}
	
	public boolean isRequireInteraction()
	{
		return this.requireInteraction;
	}
	
	public LocalNotificationOptions setRequireInteraction(final boolean requireInteraction)
	{
		this.requireInteraction = requireInteraction;
		return this;
	}
	
	public boolean isSilent()
	{
		return this.silent;
	}
	
	public LocalNotificationOptions setSilent(final boolean silent)
	{
		this.silent = silent;
		return this;
	}
	
	public String getTag()
	{
		return this.tag;
	}
	
	public LocalNotificationOptions setTag(final String tag)
	{
		this.tag = tag;
		return this;
	}
	
	public long getTimestamp()
	{
		return this.timestamp;
	}
	
	public LocalNotificationOptions setTimestamp(final long timestamp)
	{
		this.timestamp = timestamp;
		return this;
	}
	
	public int[] getVibrate()
	{
		return this.vibrate;
	}
	
	public LocalNotificationOptions setVibrate(final int... vibrate)
	{
		this.vibrate = vibrate;
		return this;
	}
	
	public LocalNotificationOptions addAction(final Action action)
	{
		this.actions.add(action);
		return this;
	}
	
	public List<Action> getActions()
	{
		return this.actions;
	}
	
	public enum Direction
	{
		auto,
		ltr,
		rtl
	}
	
	public static class Action
	{
		private String action;
		private String title;
		private String icon;
		
		public Action()
		{
		}
		
		public Action(final String action, final String title, final String icon)
		{
			this.action = action;
			this.title  = title;
			this.icon   = icon;
		}
		
		public String getAction()
		{
			return this.action;
		}
		
		public Action setAction(final String action)
		{
			this.action = action;
			return this;
		}
		
		public String getTitle()
		{
			return this.title;
		}
		
		public Action setTitle(final String title)
		{
			this.title = title;
			return this;
		}
		
		public String getIcon()
		{
			return this.icon;
		}
		
		public Action setIcon(final String icon)
		{
			this.icon = icon;
			return this;
		}
	}
}
