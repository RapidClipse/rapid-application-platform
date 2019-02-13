/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.net;


import java.io.Serializable;
import java.util.Locale;
import java.util.TimeZone;

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
	 * Gets the height of the screen in pixels. This is the full screen
	 * resolution and not the height available for the application.
	 *
	 * @return the height of the screen in pixels.
	 */
	public int getScreenHeight();
	
	
	/**
	 * Gets the width of the screen in pixels. This is the full screen
	 * resolution and not the width available for the application.
	 *
	 * @return the width of the screen in pixels.
	 */
	public int getScreenWidth();
	
	
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
	 * Returns the browser-reported TimeZone offset in milliseconds from GMT
	 * ignoring possible daylight saving adjustments that may be in effect in
	 * the browser.
	 * <p>
	 * You can use this to figure out which TimeZones the user could actually be
	 * in by calling {@link TimeZone#getAvailableIDs(int)}.
	 * </p>
	 * <p>
	 * If {@link #getRawTimezoneOffset()} and {@link #getTimezoneOffset()}
	 * returns the same value, the browser is either in a zone that does not
	 * currently have daylight saving time, or in a zone that never has daylight
	 * saving time.
	 * </p>
	 *
	 * @return timezone offset in milliseconds excluding DST, 0 if not available
	 */
	public int getRawTimezoneOffset();
	
	
	/**
	 * Returns the offset in milliseconds between the browser's GMT TimeZone and
	 * DST.
	 *
	 * @return the number of milliseconds that the TimeZone shifts when DST is
	 *         in effect
	 */
	public int getDSTSavings();
	
	
	/**
	 * Returns the browser-reported TimeZone offset in milliseconds from GMT.
	 * This includes possible daylight saving adjustments, to figure out which
	 * TimeZone the user actually might be in, see
	 * {@link #getRawTimezoneOffset()}.
	 *
	 * @see WebBrowser#getRawTimezoneOffset()
	 * @return timezone offset in milliseconds, 0 if not available
	 */
	public int getTimezoneOffset();
	
	
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


	public static ClientInfo New(final VaadinRequest request)
	{
		return new Implementation(request);
	}



	public static class Implementation implements ClientInfo
	{
		private final UserAgent		userAgent;
		private final WebBrowser	webBrowser;


		public Implementation(final VaadinRequest request)
		{
			this.userAgent = UserAgent.parseUserAgentString(request.getHeader("user-agent"));

			this.webBrowser = new WebBrowser();
			this.webBrowser.updateRequestDetails(request);
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
		public int getScreenHeight()
		{
			return this.webBrowser.getScreenHeight();
		}
		
		
		@Override
		public int getScreenWidth()
		{
			return this.webBrowser.getScreenWidth();
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
		
		
		@Override
		public int getTimezoneOffset()
		{
			return this.webBrowser.getTimezoneOffset();
		}


		@Override
		public int getRawTimezoneOffset()
		{
			return this.webBrowser.getRawTimezoneOffset();
		}
		
		
		@Override
		public int getDSTSavings()
		{
			return this.webBrowser.getDSTSavings();
		}
	}
}
