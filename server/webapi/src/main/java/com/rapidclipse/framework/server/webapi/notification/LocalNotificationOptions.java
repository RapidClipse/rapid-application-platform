
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
