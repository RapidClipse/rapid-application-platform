
package com.rapidclipse.framework.server.webapi.online;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;
import com.vaadin.flow.templatemodel.TemplateModel;


/**
 * With this class you can check the current online status of the device. More importantly you can register listeners
 * for when the device goes online. Currently the onOffline listeners are only triggered once the device comes back
 * online, as the event has to be received from the client.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@HtmlImport("frontend://webapi/online.html")
@Tag("rap-online")
public class Online extends JavascriptTemplate<Online.OnlineTemplateModel>
{
	public Online(final HasElement parent)
	{
		super(parent);
	}
	
	/**
	 * Register an online listener. This listener is triggered when the client comes back online after being
	 * offline or vice versa. To unregister this event you can either use the returned Registration or by calling
	 * {@link #unregisterAllOnlineListeners()}.
	 */
	public Registration addOnlineListener(final SerializableConsumer<OnlineState> onOnline)
	{
		final Runnable firstAddedCallback  = () -> this.getElement().callJsFunction("registerOnlineListener");
		final Runnable lastRemovedCallback = () -> this.getElement().callJsFunction("unregisterOnlineListener");
		return this.registerConsumer(OnlineState.class, onOnline, firstAddedCallback, lastRemovedCallback);
	}
	
	/**
	 * This method can be used to unregister previously registered onOnLine listeners. This will also stop the client
	 * from sending more events to the server. To register an onOnLine listener
	 * you can call the {@link #addOnLineListener(Runnable)} method.
	 */
	public void unregisterAllOnlineListeners()
	{
		final Runnable unregisterCallback = () -> this.getElement().callJsFunction("unregisterOnlineListener");
		this.unregisterAllConsumers(OnlineState.class, unregisterCallback);
	}
	
	/**
	 * Get the current OnLine state of the device (The state can not be retrieved while the client is offline).
	 *
	 * @param onStateReceived
	 *            The callback triggered when the state is received. It consumes the current onLine state.
	 */
	public static void getOnlineState(final SerializableConsumer<Boolean> onStateReceived)
	{
		UI.getCurrent().getPage().executeJs("return navigator.onLine").then(Boolean.class, onStateReceived);
	}
	
	@ClientCallable
	private void onOnline()
	{
		this.notifyConsumers(OnlineState.class, OnlineState.ONLINE);
	}
	
	@ClientCallable
	private void onOffline()
	{
		this.notifyConsumers(OnlineState.class, OnlineState.OFFLINE);
	}
	
	public static interface OnlineTemplateModel extends TemplateModel
	{
	}
}
