
package com.rapidclipse.framework.server.mobilekit.geolocation;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * This service provides information about the device's location, such as
 * latitude and longitude. Common sources of location information include Global
 * Positioning System (GPS) and location inferred from network signals such as
 * IP address, RFID, WiFi and Bluetooth MAC addresses, and GSM/CDMA cell IDs.
 * There is no guarantee that the API returns the device's actual location.
 * <p>
 * <b>WARNING</b>:<br>
 * Collection and use of geolocation data raises important privacy issues. Your
 * app's privacy policy should discuss how the app uses geolocation data,
 * whether it is shared with any other parties, and the level of precision of
 * the data (for example, coarse, fine, ZIP code level, etc.). Geolocation data
 * is generally considered sensitive because it can reveal user's whereabouts
 * and, if stored, the history of their travels. Therefore, in addition to the
 * app's privacy policy, you should strongly consider providing a just-in-time
 * notice before the app accesses geolocation data (if the device operating
 * system doesn't do so already). That notice should provide the same
 * information noted above, as well as obtaining the user's permission (e.g., by
 * presenting choices for OK and No Thanks). For more information, please see
 * the <a href=
 * "http://cordova.apache.org/docs/en/latest/guide/appdev/privacy/index.html">
 * Privacy Guide</a>.
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(GeolocationComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-geolocation", spec = "4.0.1"))
public interface GeolocationService extends MobileService
{
	public static GeolocationService getCurrent()
	{
		return MobileService.getCurrent(GeolocationService.class);
	}
	
	/**
	 * Asynchronously acquires the current position.
	 *
	 * @param options
	 *            optional options
	 * @param successCallback
	 *            The function to call when the position data is available
	 */
	default public void getCurrentPosition(
		final GeolocationOptions options,
		final Consumer<Position> successCallback)
	{
		getCurrentPosition(options, successCallback, null);
	}
	
	/**
	 * Asynchronously acquires the current position.
	 *
	 * @param options
	 *            optional options
	 * @param successCallback
	 *            The function to call when the position data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            heading position.
	 */
	public void getCurrentPosition(
		GeolocationOptions options,
		Consumer<Position> successCallback,
		Consumer<GeolocationServiceError> errorCallback);
	
	/**
	 *
	 * Asynchronously watches the geolocation for changes to geolocation. When a
	 * change occurs, the successCallback is called with the new location.
	 *
	 * @param options
	 *            optional options
	 * @param successCallback
	 *            The function to call each time the location data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            location data.
	 */
	default public void watchPosition(
		final GeolocationOptions options,
		final Consumer<PositionWatch> successCallback)
	{
		watchPosition(options, successCallback, null);
	}
	
	/**
	 *
	 * Asynchronously watches the geolocation for changes to geolocation. When a
	 * change occurs, the successCallback is called with the new location.
	 *
	 * @param options
	 *            optional options
	 * @param successCallback
	 *            The function to call each time the location data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            location data.
	 */
	public void watchPosition(
		GeolocationOptions options,
		Consumer<PositionWatch> successCallback,
		Consumer<GeolocationServiceError> errorCallback);
}
