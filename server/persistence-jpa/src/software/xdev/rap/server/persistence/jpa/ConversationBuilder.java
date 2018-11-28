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

package software.xdev.rap.server.persistence.jpa;


import java.util.ArrayList;
import java.util.List;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.AfterNavigationListener;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public abstract class ConversationBuilder
{
	public static UIBoundConversationBuilder uiBound(final UI ui)
	{
		return new UIBoundConversationBuilder(ui);
	}


	public abstract Conversation startConversation();



	public static class UIBoundConversationBuilder extends ConversationBuilder
	{
		private final UI							ui;
		private String								persistenceUnit;
		private AfterNavigationListener				afterNavigationListener;
		private Registration						afterNavigationRegistration;
		private List<String>						allowedNavigationViews;
		private ComponentEventListener<DetachEvent>	detachListener;
		private List<Registration>					detachRegistrations;


		public UIBoundConversationBuilder(final UI ui)
		{
			this.ui = ui;
		}


		public UIBoundConversationBuilder persistenceUnit(final String persistenceUnit)
		{
			this.persistenceUnit = persistenceUnit;
			return this;
		}


		public UIBoundConversationBuilder endOnDetach(final Component... components)
		{
			if(this.detachListener == null)
			{
				this.detachListener = event -> endConversation();
				this.detachRegistrations = new ArrayList<>();
			}

			for(final Component c : components)
			{
				this.detachRegistrations.add(c.addDetachListener(this.detachListener));
			}

			return this;
		}


		public UIBoundConversationBuilder endOnNavigateOut(final String... views)
		{
			if(this.allowedNavigationViews == null)
			{
				this.allowedNavigationViews = new ArrayList<>();
			}
			for(final String view : views)
			{
				this.allowedNavigationViews.add(view);
			}

			if(this.afterNavigationListener == null)
			{
				this.afterNavigationListener = event -> {

					final String path = event.getLocation().getPath();
					if(!this.allowedNavigationViews.contains(path))
					{
						endConversation();
					}
				};

				this.afterNavigationRegistration = this.ui
						.addAfterNavigationListener(this.afterNavigationListener);
			}

			return this;
		}


		private void endConversation()
		{
			if(this.detachListener != null)
			{
				this.detachRegistrations.forEach(Registration::remove);
				this.detachRegistrations.clear();
				this.detachRegistrations = null;
				this.detachListener = null;
			}

			if(this.afterNavigationListener != null)
			{
				this.afterNavigationRegistration.remove();
				this.afterNavigationRegistration = null;
				this.allowedNavigationViews.clear();
				this.allowedNavigationViews = null;
				this.afterNavigationListener = null;
			}

			if(this.persistenceUnit != null)
			{
				ConversationUtils.endConversation(this.persistenceUnit);
			}
			else
			{
				ConversationUtils.endConversation();
			}
		}


		@Override
		public Conversation startConversation()
		{
			if(this.persistenceUnit != null)
			{
				return ConversationUtils.startConversation(this.persistenceUnit);
			}

			return ConversationUtils.startConversation();
		}
	}
}
