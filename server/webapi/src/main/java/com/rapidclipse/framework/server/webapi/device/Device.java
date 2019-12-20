
package com.rapidclipse.framework.server.webapi.device;

import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.UI;


/*
 * Collection of functionalities that don't belong anywhere specific and are too small to be grouped together into
 * something seperate.
 */
/**
 * Various device functionalities.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public final class Device
{
	/**
	 * Let the device vibrate with the given pattern.<br>
	 *
	 * @param vibrationPattern
	 *            The pattern is built as follows: length, pause, length, pause etc.<br>
	 *            For example 100, 200, 300 would vibrate for 100ms then pause for 200 and then vibrate for 300ms again.
	 */
	public static void vibrate(final int... vibrationPattern)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("navigator.vibrate($0)", JsonUtils.encodeObject(vibrationPattern));
	}

	/**
	 * Let the user select to share the given url to a device specific list of apps. If it is shared e.g. to the
	 * clipboard, only the url will be copied.
	 *
	 * @param url
	 *            The url that should be shared.
	 * @param title
	 *            A title that may be displayed.
	 * @param text
	 *            A description that may be displayed.
	 */
	public static void shareUrl(final String url, final String title, final String text)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("if('share' in navigator) { navigator.share({url: $0, title: $1, text: $2}); }",
				url,
				title,
				text);
	}

	private Device()
	{
		throw new Error();
	}
}
