/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server.mobilekit.app;


import java.util.ArrayList;
import java.util.List;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;

import elemental.json.JsonArray;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-app")
@HtmlImport("app.html")
public class AppComponent extends MobileComponent implements AppService
{
	private final List<AppEventHandler>	pauseHandlers				= new ArrayList<>();
	private final List<AppEventHandler>	resumeHandlers				= new ArrayList<>();
	private final List<AppEventHandler>	backButtonHandlers			= new ArrayList<>();
	private final List<AppEventHandler>	menuButtonHandlers			= new ArrayList<>();
	private final List<AppEventHandler>	searchButtonHandlers		= new ArrayList<>();
	private final List<AppEventHandler>	volumeDownButtonHandlers	= new ArrayList<>();
	private final List<AppEventHandler>	volumeUpButtonHandlers		= new ArrayList<>();


	public AppComponent()
	{
		super();
	}


	@Override
	public void closeApp()
	{
		getElement().callFunction("closeApp");
	}


	@Override
	public void addPauseHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.pauseHandlers,"addPauseHandler");
	}


	@Override
	public void removePauseHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.pauseHandlers,"removePauseHandler");
	}


	@Override
	public void addResumeHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.resumeHandlers,"addResumeHandler");
	}


	@Override
	public void removeResumeHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.resumeHandlers,"removeResumeHandler");
	}


	@Override
	public void addBackButtonHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.backButtonHandlers,"addBackButtonHandler");
	}


	@Override
	public void removeBackButtonHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.backButtonHandlers,"removeBackButtonHandler");
	}


	@Override
	public void addMenuButtonHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.menuButtonHandlers,"addMenuButtonHandler");
	}


	@Override
	public void removeMenuButtonHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.menuButtonHandlers,"removeMenuButtonHandler");
	}


	@Override
	public void addSearchButtonHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.searchButtonHandlers,"addSearchButtonHandler");
	}


	@Override
	public void removeSearchButtonHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.searchButtonHandlers,"removeSearchButtonHandler");
	}


	@Override
	public void addVolumeDownButtonHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.volumeDownButtonHandlers,"addVolumeDownButtonHandler");
	}


	@Override
	public void removeVolumeDownButtonHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.volumeDownButtonHandlers,"removeVolumeDownButtonHandler");
	}


	@Override
	public void addVolumeUpButtonHandler(final AppEventHandler handler)
	{
		addEventHandler(handler,this.volumeUpButtonHandlers,"addVolumeUpButtonHandler");
	}


	@Override
	public void removeVolumeUpButtonHandler(final AppEventHandler handler)
	{
		removeEventHandler(handler,this.volumeUpButtonHandlers,"removeVolumeUpButtonHandler");
	}


	private void addEventHandler(final AppEventHandler handler, final List<AppEventHandler> list,
			final String function)
	{
		if(list.isEmpty())
		{
			getElement().callFunction(function);
		}

		list.add(handler);
	}


	private void removeEventHandler(final AppEventHandler handler, final List<AppEventHandler> list,
			final String function)
	{
		list.remove(handler);

		if(list.isEmpty())
		{
			getElement().callFunction(function);
		}
	}


	@ClientCallable
	void onPause(final JsonArray arguments)
	{
		handleEvent(this.pauseHandlers);
	}


	@ClientCallable
	void onResume(final JsonArray arguments)
	{
		handleEvent(this.resumeHandlers);
	}


	@ClientCallable
	void onBackButton(final JsonArray arguments)
	{
		handleEvent(this.backButtonHandlers);
	}


	@ClientCallable
	void onMenuButton(final JsonArray arguments)
	{
		handleEvent(this.menuButtonHandlers);
	}


	@ClientCallable
	void onSearchButton(final JsonArray arguments)
	{
		handleEvent(this.searchButtonHandlers);
	}


	@ClientCallable
	void onVolumeDownButton(final JsonArray arguments)
	{
		handleEvent(this.volumeDownButtonHandlers);
	}


	@ClientCallable
	void onVolumeUpButton(final JsonArray arguments)
	{
		handleEvent(this.volumeUpButtonHandlers);
	}


	private void handleEvent(final List<AppEventHandler> handlers)
	{
		if(handlers.isEmpty())
		{
			return;
		}

		final AppEvent event = new AppEvent(this);

		for(int i = handlers.size(); --i >= 0;)
		{
			handlers.get(i).handleAppEvent(event);
			if(event.isConsumed())
			{
				break;
			}
		}
	}
}
