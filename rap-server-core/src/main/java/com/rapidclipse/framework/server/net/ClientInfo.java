/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.net;

import java.io.Serializable;
import java.util.Locale;

import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinSession;
import com.vaadin.flow.server.WebBrowser;

import eu.bitwalker.useragentutils.Browser;
import eu.bitwalker.useragentutils.DeviceType;
import eu.bitwalker.useragentutils.OperatingSystem;
import eu.bitwalker.useragentutils.UserAgent;


/**
 * Information class for user-agent information with operating system and
 * browser details.
 *
 * @author XDEV Software
 *
 * @see WebBrowser
 * @see UserAgent
 */
public interface ClientInfo extends Serializable
{
	/**
	 * Detects if the client is a standard desktop or laptop computer
	 *
	 * @see #isTablet()
	 * @see #isMobile()
	 */
	public boolean isComputer();

	/**
	 * Detects if the client is a small tablet type computer
	 *
	 * @see #isComputer()
	 * @see #isMobile()
	 */
	public boolean isTablet();

	/**
	 * Detects if the client is a mobile phone or similar small mobile device
	 *
	 * @see #isComputer()
	 * @see #isTablet()
	 */
	public boolean isMobile();

	/**
	 * Detects if the client is run on Android.
	 */
	public boolean isAndroid();

	/**
	 * Detects if the client is run on iOS.
	 */
	public boolean isIOS();

	/**
	 * Detects if the client is run on MacOS-X.
	 */
	public boolean isMacOSX();

	/**
	 * Detects if the client is run on Windows.
	 */
	public boolean isWindows();

	/**
	 * Detects if the client is run on Linux.
	 */
	public boolean isLinux();

	/**
	 * Detects if the client is using the Chrome browser.
	 */
	public boolean isChrome();

	/**
	 * Detects if the client is using the Firefox browser.
	 */
	public boolean isFirefox();

	/**
	 * Detects if the client is using the Internet Explorer browser.
	 */
	public boolean isInternetExplorer();

	/**
	 * Detects if the client is using the Edge browser.
	 */
	public boolean isEdge();

	/**
	 * Detects if the client is using the Safari browser.
	 */
	public boolean isSafari();

	/**
	 * Detects if the client is using the Operabrowser.
	 */
	public boolean isOpera();

	/**
	 * Returns the IP-address of the client. If the application is running
	 * inside a portlet, this method will return null. *
	 */
	public String getAddress();

	/**
	 * Returns the locale of the client browser.
	 */
	public Locale getLocale();

	/**
	 * Gets the currently used session's client info.
	 *
	 * @return the current client info if available, otherwise <code>null</code>
	 */
	public static ClientInfo getCurrent()
	{
		final VaadinSession session = VaadinSession.getCurrent();
		return session != null ? session.getAttribute(ClientInfo.class) : null;
	}

	public static ClientInfo New(
		final VaadinSession session,
		final VaadinRequest request)
	{
		return new Default(session, request);
	}

	public static class Default implements ClientInfo
	{
		private final UserAgent  userAgent;
		private final WebBrowser webBrowser;

		protected Default(
			final VaadinSession session,
			final VaadinRequest request)
		{
			this.userAgent  = UserAgent.parseUserAgentString(request.getHeader("user-agent"));

			this.webBrowser = session.getBrowser();
		}

		@Override
		public boolean isComputer()
		{
			return this.userAgent.getOperatingSystem().getDeviceType() == DeviceType.COMPUTER;
		}

		@Override
		public boolean isTablet()
		{
			return this.userAgent.getOperatingSystem().getDeviceType() == DeviceType.TABLET;
		}

		@Override
		public boolean isMobile()
		{
			return this.userAgent.getOperatingSystem().getDeviceType() == DeviceType.MOBILE;
		}

		@Override
		public boolean isAndroid()
		{
			return isOperatingSystem(OperatingSystem.ANDROID);
		}

		@Override
		public boolean isIOS()
		{
			return isOperatingSystem(OperatingSystem.IOS);
		}

		@Override
		public boolean isMacOSX()
		{
			return isOperatingSystem(OperatingSystem.MAC_OS_X);
		}

		@Override
		public boolean isWindows()
		{
			return isOperatingSystem(OperatingSystem.WINDOWS);
		}

		@Override
		public boolean isLinux()
		{
			return isOperatingSystem(OperatingSystem.LINUX);
		}

		private boolean isOperatingSystem(final OperatingSystem test)
		{
			final OperatingSystem os = this.userAgent.getOperatingSystem();
			return os == test || os.getGroup() == test;
		}

		@Override
		public boolean isChrome()
		{
			return isBrowser(Browser.CHROME);
		}

		@Override
		public boolean isFirefox()
		{
			return isBrowser(Browser.FIREFOX);
		}

		@Override
		public boolean isInternetExplorer()
		{
			return isBrowser(Browser.IE);
		}

		@Override
		public boolean isEdge()
		{
			return isBrowser(Browser.EDGE);
		}

		@Override
		public boolean isSafari()
		{
			return isBrowser(Browser.SAFARI);
		}

		@Override
		public boolean isOpera()
		{
			return isBrowser(Browser.OPERA);
		}

		private boolean isBrowser(final Browser test)
		{
			final Browser browser = this.userAgent.getBrowser();
			return browser == test || browser.getGroup() == test;
		}

		@Override
		public String getAddress()
		{
			return this.webBrowser.getAddress();
		}

		@Override
		public Locale getLocale()
		{
			return this.webBrowser.getLocale();
		}
	}
}
