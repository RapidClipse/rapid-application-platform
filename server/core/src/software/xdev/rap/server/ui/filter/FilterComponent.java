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

package software.xdev.rap.server.ui.filter;


import java.util.List;

import com.vaadin.flow.component.AbstractCompositeField;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;

import software.xdev.rap.server.data.filter.Composite.Connector;


/**
 * @author XDEV Software
 *
 */
public class FilterComponent
		extends AbstractCompositeField<VerticalLayout, FilterComponent, FilterData>
		implements FilterContext, HasSize
{
	private boolean					caseSensitive				= false;
	private char					wildcard					= '*';

	private Connector				searchPropertiesConnector	= Connector.OR;
	private Connector				searchMultiWordConnector	= Connector.OR;
	private Connector				filterPropertiesConnector	= Connector.AND;
	private Connector				searchAndFilterConnector	= Connector.AND;

	private SearchFilterGenerator	searchFilterGenerator		= SearchFilterGenerator.New();

	private FilterOperatorRegistry	filterOperatorRegistry		= FilterOperatorRegistry.Default();

	private FilterSubject			filterSubject;

	private TextField				searchTextField;
	private Button					addFilterButton;
	private List<FilterEntryEditor>	entryEditors;


	public FilterComponent()
	{
		super(new FilterData("",null));
	}


	@Override
	public boolean isCaseSensitive()
	{
		return this.caseSensitive;
	}


	public void setCaseSensitive(final boolean caseSensitive)
	{
		this.caseSensitive = caseSensitive;
	}


	@Override
	public char getWildcard()
	{
		return this.wildcard;
	}


	public void setWildcard(final char wildcard)
	{
		this.wildcard = wildcard;
	}


	@Override
	public Connector getSearchPropertiesConnector()
	{
		return this.searchPropertiesConnector;
	}


	public void setSearchPropertiesConnector(final Connector searchPropertiesConnector)
	{
		this.searchPropertiesConnector = searchPropertiesConnector;
	}


	@Override
	public Connector getSearchMultiWordConnector()
	{
		return this.searchMultiWordConnector;
	}


	public void setSearchMultiWordConnector(final Connector searchMultiWordConnector)
	{
		this.searchMultiWordConnector = searchMultiWordConnector;
	}


	@Override
	public Connector getFilterPropertiesConnector()
	{
		return this.filterPropertiesConnector;
	}


	public void setFilterPropertiesConnector(final Connector filterPropertiesConnector)
	{
		this.filterPropertiesConnector = filterPropertiesConnector;
	}


	@Override
	public Connector getSearchAndFilterConnector()
	{
		return this.searchAndFilterConnector;
	}


	public void setSearchAndFilterConnector(final Connector searchAndFilterConnector)
	{
		this.searchAndFilterConnector = searchAndFilterConnector;
	}


	public SearchFilterGenerator getSearchFilterGenerator()
	{
		return this.searchFilterGenerator;
	}


	public void setSearchFilterGenerator(final SearchFilterGenerator searchFilterGenerator)
	{
		this.searchFilterGenerator = searchFilterGenerator;
	}


	@Override
	public FilterOperatorRegistry getFilterOperatorRegistry()
	{
		return this.filterOperatorRegistry;
	}


	public void setFilterOperatorRegistry(final FilterOperatorRegistry filterOperatorRegistry)
	{
		this.filterOperatorRegistry = filterOperatorRegistry;
	}


	@Override
	public FilterSubject getFilterSubject()
	{
		return this.filterSubject;
	}


	public void setFilterSubject(final FilterSubject filterSubject)
	{
		this.filterSubject = filterSubject;
	}


	@Override
	protected VerticalLayout initContent()
	{
		this.searchTextField = createSearchTextField();
		this.searchTextField.addValueChangeListener(event -> updateFilter());
		this.searchTextField.setEnabled(false);

		this.addFilterButton = createAddFilterButton();
		this.addFilterButton.addClickListener(event -> addFilterEntry(0));
		this.addFilterButton.setEnabled(false);

		final HorizontalLayout searchBar = new HorizontalLayout(this.searchTextField,
				this.addFilterButton);
		searchBar.expand(this.searchTextField);
		searchBar.setWidth("100%");

		final VerticalLayout content = new VerticalLayout(searchBar);
		content.setPadding(false);
		content.setSpacing(true);
		return content;
	}


	protected TextField createSearchTextField()
	{
		final TextField textField = new TextField();
		textField.setValueChangeMode(ValueChangeMode.EAGER);
		return textField;
	}


	protected Button createAddFilterButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.PLUS.create());
		return button;
	}


	protected void addFilterEntry(final int index)
	{

	}


	protected void updateFilter()
	{
		// setModelValue()
		System.out.println("Updating Filter ...");
	}


	@Override
	protected void setPresentationValue(final FilterData newPresentationValue)
	{
	}
}
