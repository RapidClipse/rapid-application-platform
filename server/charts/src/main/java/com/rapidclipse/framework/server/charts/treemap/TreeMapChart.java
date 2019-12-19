/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts.treemap;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasTitle;
import com.rapidclipse.framework.server.charts.TextStyle;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("treemap-chart")
public class TreeMapChart extends AbstractChart
	implements HasFont, AllowsIFrame, HasTitle
{
	public TreeMapChart()
	{
		super("TreeMap", "treemap");
	}

	public ChartModel
		initDefaultColumns(
			final String idColumn,
			final String parentIdColumn,
			final String sizeColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, idColumn))
			.addColumn(Column.New(Column.Type.STRING, parentIdColumn))
			.addColumn(Column.New(Column.Type.NUMBER, sizeColumn));
	}

	public ChartModel
		initDefaultColumns(
			final String idColumn,
			final String parentIdColumn,
			final String sizeColumn,
			final String colorColumn)
	{
		return initDefaultColumns(idColumn, parentIdColumn, sizeColumn)
			.addColumn(Column.New(Column.Type.NUMBER, colorColumn));
	}

	public String getFontColor()
	{
		return properties().get("fontColor", "#ffffff");
	}

	public void setFontColor(final String fontColor)
	{
		properties().put("headerColor", fontColor);
	}

	public String getHeaderColor()
	{
		return properties().get("headerColor", "#988f86");
	}

	public void setHeaderColor(final String headerColor)
	{
		properties().put("headerColor", headerColor);
	}

	public Integer getHeaderHeight()
	{
		return properties().get("headerHeight", 0);
	}

	public void setHeaderHeight(final Integer headerHeight)
	{
		properties().put("headerHeight", headerHeight);
	}

	public String getHeaderHighlightColor()
	{
		return properties().get("headerHighlightColor", null);
	}

	public void setHeaderHighlightColor(final String headerHighlightColor)
	{
		properties().put("headerHighlightColor", headerHighlightColor);
	}

	public boolean getHighlightOnMouseOver()
	{
		return properties().get("highlightOnMouseOver", false);
	}

	public void setHighlightOnMouseOver(final boolean highlightOnMouseOver)
	{
		properties().put("highlightOnMouseOver", highlightOnMouseOver);
	}

	public Number getHintOpacity()
	{
		return properties().get("hintOpacity", 0.0);
	}

	public void setHintOpacity(final Number hintOpacity)
	{
		properties().put("hintOpacity", hintOpacity);
	}

	public String getMaxColor()
	{
		return properties().get("maxColor", "#00dd00");
	}

	public void setMaxColor(final String maxColor)
	{
		properties().put("maxColor", maxColor);
	}

	public int getMaxDepth()
	{
		return properties().get("maxDepth", 1);
	}

	public void setMaxDepth(final int maxDepth)
	{
		properties().put("maxDepth", maxDepth);
	}

	public String getMaxHighlightColor()
	{
		return properties().get("maxHighlightColor", null);
	}

	public void setMaxHighlightColor(final String maxHighlightColor)
	{
		properties().put("maxHighlightColor", maxHighlightColor);
	}

	public int getMaxPostDepth()
	{
		return properties().get("maxPostDepth", 0);
	}

	public void setMaxPostDepth(final int maxPostDepth)
	{
		properties().put("maxPostDepth", maxPostDepth);
	}

	public Integer getMaxColorValue()
	{
		return properties().get("maxColorValue", null);
	}

	public void setMaxColorValue(final Integer maxColorValue)
	{
		properties().put("maxColorValue", maxColorValue);
	}

	public String getMidColor()
	{
		return properties().get("midColor", "#000000");
	}

	public void setMidColor(final String midColor)
	{
		properties().put("midColor", midColor);
	}

	public String getMidHighlightColor()
	{
		return properties().get("midHighlightColor", null);
	}

	public void setMidHighlightColor(final String midHighlightColor)
	{
		properties().put("midHighlightColor", midHighlightColor);
	}

	public String getMinColor()
	{
		return properties().get("minColor", "#dd0000");
	}

	public void setMinColor(final String minColor)
	{
		properties().put("minColor", minColor);
	}

	public String getMinHighlightColor()
	{
		return properties().get("minHighlightColor", null);
	}

	public void setMinHighlightColor(final String minHighlightColor)
	{
		properties().put("minHighlightColor", minHighlightColor);
	}

	public Number getMinColorValue()
	{
		return properties().get("minColorValue", null);
	}

	public void setMinColorValue(final Number minColorValue)
	{
		properties().put("minColorValue", minColorValue);
	}

	public String getNoColor()
	{
		return properties().get("noColor", "#000000");
	}

	public void setNoColor(final String noColor)
	{
		properties().put("noColor", noColor);
	}

	public String getNoHighlightColor()
	{
		return properties().get("noHighlightColor", null);
	}

	public void setNoHighlightColor(final String noHighlightColor)
	{
		properties().put("noHighlightColor", noHighlightColor);
	}

	public boolean getShowScale()
	{
		return properties().get("showScale", false);
	}

	public void setShowScale(final boolean showScale)
	{
		properties().put("showScale", showScale);
	}

	public boolean getShowTooltips()
	{
		return properties().get("showTooltips", true);
	}

	public void setShowTooltips(final boolean showTooltips)
	{
		properties().put("showTooltips", showTooltips);
	}

	public TextStyle getTextStyle()
	{
		return properties().get("textStyle", null);
	}

	public void setTextStyle(final TextStyle textStyle)
	{
		properties().put("textStyle", textStyle);
	}

	public boolean getUseWeightedAverageForAggregation()
	{
		return properties().get("useWeightedAverageForAggregation", false);
	}

	public void setUseWeightedAverageForAggregation(final boolean useWeightedAverageForAggregation)
	{
		properties().put("useWeightedAverageForAggregation", useWeightedAverageForAggregation);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns("ID", "Parent", "Number Of Lines")
			.addRow("Shakespeare", null, 0)
			.addRow("Comedies", "Shakespeare", null)
			.addRow("Tragedies", "Shakespeare", null)
			.addRow("Histories", "Shakespeare", null)
			.addRow("As You Like It", "Comedies", null)
			.addRow("Adam", "As You Like It", 10)
			.addRow("Amiens", "As You Like It", 10)
			.addRow("Audrey", "As You Like It", 12)
			.addRow("Celia", "As You Like It", 108)
			.addRow("Charles", "As You Like It", 8)
			.addRow("Corin", "As You Like It", 24)
			.addRow("Dennis", "As You Like It", 2)
			.addRow("Duke", "As You Like It", 32)
			.addRow("Frederick", "As You Like It", 20)
			.addRow("Hymen", "As You Like It", 1)
			.addRow("Jaques (lord)", "As You Like It", 57)
			.addRow("Jaques (son)", "As You Like It", 2)
			.addRow("Le Beau", "As You Like It", 14)
			.addRow("Oliver", "As You Like It", 37)
			.addRow("Orlando", "As You Like It", 120)
			.addRow("Phebe", "As You Like It", 23)
			.addRow("Rosalind", "As You Like It", 201)
			.addRow("Silvius", "As You Like It", 24)
			.addRow("Sir Oliver Martext", "As You Like It", 3)
			.addRow("Touchstone", "As You Like It", 74)
			.addRow("William", "As You Like It", 11)
			.addRow("Comedy Of Errors", "Comedies", null)
			.addRow("Adriana", "Comedy Of Errors", 79)
			.addRow("Aegeon", "Comedy Of Errors", 17)
			.addRow("Aemilia", "Comedy Of Errors", 16)
			.addRow("Angelo", "Comedy Of Errors", 31)
			.addRow("Antipholus of Ephesus", "Comedy Of Errors", 76)
			.addRow("Antipholus of Syracuse", "Comedy Of Errors", 103)
			.addRow("Balthazar", "Comedy Of Errors", 5)
			.addRow("Courtezan", "Comedy Of Errors", 11)
			.addRow("Dromio of Ephesus", "Comedy Of Errors", 63)
			.addRow("Dromio of Syracuse", "Comedy Of Errors", 99)
			.addRow("Luce", "Comedy Of Errors", 7)
			.addRow("Luciana", "Comedy Of Errors", 43)
			.addRow("Pinch", "Comedy Of Errors", 6)
			.addRow("Solinus", "Comedy Of Errors", 22)
			.addRow("Merchant Of Venice", "Comedies", null)
			.addRow("Antonio", "Merchant Of Venice", 47)
			.addRow("Balthasar", "Merchant Of Venice", 1)
			.addRow("Bassanio", "Merchant Of Venice", 73)
			.addRow("Duke (of Venice)", "Merchant Of Venice", 18)
			.addRow("Gratiano", "Merchant Of Venice", 48)
			.addRow("Jessica", "Merchant Of Venice", 26)
			.addRow("Launcelot Gobbo", "Merchant Of Venice", 44)
			.addRow("Leonardo", "Merchant Of Venice", 2)
			.addRow("Lorenzo", "Merchant Of Venice", 47)
			.addRow("Nerissa", "Merchant Of Venice", 36)
			.addRow("Old Gobbo", "Merchant Of Venice", 19)
			.addRow("Portia", "Merchant Of Venice", 117)
			.addRow("Prince of Arragon", "Merchant Of Venice", 4)
			.addRow("Prince of Morocco", "Merchant Of Venice", 7)
			.addRow("Salanio", "Merchant Of Venice", 18)
			.addRow("Salarino", "Merchant Of Venice", 27)
			.addRow("Salerio", "Merchant Of Venice", 6)
			.addRow("Shylock", "Merchant Of Venice", 79)
			.addRow("Stephano", "Merchant Of Venice", 3)
			.addRow("Tubal", "Merchant Of Venice", 8)
			.addRow("Midsummer Night's Dream", "Comedies", null)
			.addRow("Bottom", "Midsummer Night's Dream", 59)
			.addRow("Cobweb", "Midsummer Night's Dream", 4)
			.addRow("Demetrius", "Midsummer Night's Dream", 48)
			.addRow("Egeus", "Midsummer Night's Dream", 7)
			.addRow("Fairy", "Midsummer Night's Dream", 4)
			.addRow("Flute", "Midsummer Night's Dream", 18)
			.addRow("Helena", "Midsummer Night's Dream", 36)
			.addRow("Hermia", "Midsummer Night's Dream", 48)
			.addRow("Hippolyta", "Midsummer Night's Dream", 14)
			.addRow("Lysander", "Midsummer Night's Dream", 50)
			.addRow("Moth", "Midsummer Night's Dream", 2)
			.addRow("Mustardseed", "Midsummer Night's Dream", 5)
			.addRow("Oberon", "Midsummer Night's Dream", 29)
			.addRow("Peaseblossom", "Midsummer Night's Dream", 4)
			.addRow("Philostrate", "Midsummer Night's Dream", 6)
			.addRow("Puck", "Midsummer Night's Dream", 33)
			.addRow("Quince", "Midsummer Night's Dream", 40)
			.addRow("Snout", "Midsummer Night's Dream", 9)
			.addRow("Snug", "Midsummer Night's Dream", 4)
			.addRow("Starveling", "Midsummer Night's Dream", 7)
			.addRow("Theseus", "Midsummer Night's Dream", 48)
			.addRow("Titania", "Midsummer Night's Dream", 23)
			.addRow("Taming Of The Shrew", "Comedies", null)
			.addRow("Baptista Minola", "Taming Of The Shrew", 68)
			.addRow("Bianca", "Taming Of The Shrew", 29)
			.addRow("Biondello", "Taming Of The Shrew", 39)
			.addRow("Christopher Sly", "Taming Of The Shrew", 24)
			.addRow("Curtis", "Taming Of The Shrew", 20)
			.addRow("Gremio", "Taming Of The Shrew", 58)
			.addRow("Grumio", "Taming Of The Shrew", 63)
			.addRow("Haberdasher", "Taming Of The Shrew", 1)
			.addRow("Hortensio", "Taming Of The Shrew", 70)
			.addRow("Joseph", "Taming Of The Shrew", 1)
			.addRow("Katherina", "Taming Of The Shrew", 82)
			.addRow("Lucentio", "Taming Of The Shrew", 61)
			.addRow("Nathaniel", "Taming Of The Shrew", 4)
			.addRow("Nicholas", "Taming Of The Shrew", 1)
			.addRow("Peter", "Taming Of The Shrew", 2)
			.addRow("Petruchio", "Taming Of The Shrew", 158)
			.addRow("Philip", "Taming Of The Shrew", 1)
			.addRow("Tranio", "Taming Of The Shrew", 90)
			.addRow("Vincentio", "Taming Of The Shrew", 23)
			.addRow("The Tempest", "Comedies", null)
			.addRow("Adrian", "The Tempest", 9)
			.addRow("Alonso", "The Tempest", 40)
			.addRow("Antonio, duke of Milan", "The Tempest", 57)
			.addRow("Ariel", "The Tempest", 45)
			.addRow("Caliban", "The Tempest", 50)
			.addRow("Ceres", "The Tempest", 4)
			.addRow("Ferdinand", "The Tempest", 31)
			.addRow("Francisco", "The Tempest", 2)
			.addRow("Gonzalo", "The Tempest", 52)
			.addRow("Iris", "The Tempest", 4)
			.addRow("Juno", "The Tempest", 2)
			.addRow("Master", "The Tempest", 2)
			.addRow("Miranda", "The Tempest", 50)
			.addRow("Nymphs", "The Tempest", 0)
			.addRow("Prospero", "The Tempest", 114)
			.addRow("Reapers", "The Tempest", 0)
			.addRow("Sebastian", "The Tempest", 67)
			.addRow("Stephano (Servant to Portia)", "The Tempest", 60)
			.addRow("Trinculo", "The Tempest", 39)
			.addRow("Henry VIII", "Histories", null)
			.addRow("Anne Bullen", "Henry VIII", 18)
			.addRow("Archbishop Cranmer", "Henry VIII", 21)
			.addRow("Bishop Lincoln", "Henry VIII", 2)
			.addRow("Brandon", "Henry VIII", 6)
			.addRow("Capucius", "Henry VIII", 5)
			.addRow("Cardinal Campeius", "Henry VIII", 14)
			.addRow("Cardinal Wolsey", "Henry VIII", 79)
			.addRow("Cromwell", "Henry VIII", 21)
			.addRow("Doctor Butts", "Henry VIII", 4)
			.addRow("Duke of Buckingham", "Henry VIII", 26)
			.addRow("Duke of Norfolk", "Henry VIII", 48)
			.addRow("Duke of Suffolk", "Henry VIII", 30)
			.addRow("Earl of Surrey", "Henry VIII", 24)
			.addRow("First Secretary to Wolsey", "Henry VIII", 2)
			.addRow("Gardiner", "Henry VIII", 22)
			.addRow("Garter", "Henry VIII", 1)
			.addRow("Griffith", "Henry VIII", 13)
			.addRow("King Henry VIII", "Henry VIII", 81)
			.addRow("Lord Abergavenny", "Henry VIII", 5)
			.addRow("Lord Chamberlain", "Henry VIII", 38)
			.addRow("Lord Chancellor", "Henry VIII", 7)
			.addRow("Lord Sands", "Henry VIII", 17)
			.addRow("Old Lady", "Henry VIII", 14)
			.addRow("Patience", "Henry VIII", 3)
			.addRow("Porter (door-keeper of the Council-chamber)", "Henry VIII", 10)
			.addRow("Queen Katharine", "Henry VIII", 50)
			.addRow("Sir Anthony Denny", "Henry VIII", 3)
			.addRow("Sir Henry Guildford", "Henry VIII", 1)
			.addRow("Sir Nicholas Vaux", "Henry VIII", 1)
			.addRow("Sir Thomas Lovell", "Henry VIII", 21)
			.addRow("Surveyor to the Duke of Buckingham", "Henry VIII", 9)
			.addRow("History Of King John", "Histories", null)
			.addRow("Arthur Duke of Bretagne", "History Of King John", 23)
			.addRow("Blanch", "History Of King John", 9)
			.addRow("Cardinal Pandulph", "History Of King John", 23)
			.addRow("Chatillon", "History Of King John", 5)
			.addRow("Constance", "History Of King John", 36)
			.addRow("Essex", "History Of King John", 1)
			.addRow("Faulconbridge", "History Of King John", 4)
			.addRow("Hubert de Burgh", "History Of King John", 52)
			.addRow("James Gurney", "History Of King John", 1)
			.addRow("King John", "History Of King John", 95)
			.addRow("King Phillip", "History Of King John", 43)
			.addRow("Lady Faulconbridge", "History Of King John", 5)
			.addRow("Lewis the Dauphin", "History Of King John", 29)
			.addRow("Lord Bigot", "History Of King John", 6)
			.addRow("Lymoges duke of Austria", "History Of King John", 16)
			.addRow("Melun", "History Of King John", 3)
			.addRow("Pembroke earl of Pembroke", "History Of King John", 20)
			.addRow("Peter of Pomfret", "History Of King John", 1)
			.addRow("Philip the Bastard", "History Of King John", 89)
			.addRow("Prince Henry", "History Of King John", 8)
			.addRow("Queen Elinor", "History Of King John", 22)
			.addRow("Salisbury earl of Salisbury", "History Of King John", 36)
			.addRow("Antony And Cleopatra", "Tragedies", null)
			.addRow("Agrippa", "Antony And Cleopatra", 28)
			.addRow("Alexas", "Antony And Cleopatra", 15)
			.addRow("Antony", "Antony And Cleopatra", 202)
			.addRow("Canidius", "Antony And Cleopatra", 10)
			.addRow("Captain", "Antony And Cleopatra", 1)
			.addRow("Charmian", "Antony And Cleopatra", 63)
			.addRow("Cleopatra", "Antony And Cleopatra", 204)
			.addRow("Demetrius (Friend to Antony)", "Antony And Cleopatra", 2)
			.addRow("Dercetas", "Antony And Cleopatra", 5)
			.addRow("Diomedes", "Antony And Cleopatra", 7)
			.addRow("Dolabella", "Antony And Cleopatra", 23)
			.addRow("Domitius Enobarus", "Antony And Cleopatra", 113)
			.addRow("Egyptian", "Antony And Cleopatra", 2)
			.addRow("Eros", "Antony And Cleopatra", 27)
			.addRow("Euphronius", "Antony And Cleopatra", 5)
			.addRow("Gallus", "Antony And Cleopatra", 1)
			.addRow("Iras", "Antony And Cleopatra", 18)
			.addRow("Lepidus", "Antony And Cleopatra", 30)
			.addRow("Mardian", "Antony And Cleopatra", 7)
			.addRow("Mecaenas", "Antony And Cleopatra", 16)
			.addRow("Menas", "Antony And Cleopatra", 35)
			.addRow("Menecrates", "Antony And Cleopatra", 2)
			.addRow("Octavia", "Antony And Cleopatra", 13)
			.addRow("Octavius", "Antony And Cleopatra", 98)
			.addRow("Philo", "Antony And Cleopatra", 2)
			.addRow("Pompey", "Antony And Cleopatra", 41)
			.addRow("Proculeius", "Antony And Cleopatra", 10)
			.addRow("Scarus", "Antony And Cleopatra", 12)
			.addRow("Seleucus", "Antony And Cleopatra", 3)
			.addRow("Silius", "Antony And Cleopatra", 3)
			.addRow("Taurus", "Antony And Cleopatra", 1)
			.addRow("Thyreus", "Antony And Cleopatra", 12)
			.addRow("Varrius", "Antony And Cleopatra", 1)
			.addRow("Ventidius", "Antony And Cleopatra", 4)
			.addRow("Coriolanus", "Tragedies", null)
			.addRow("Aedile", "Coriolanus", 10)
			.addRow("Cominius", "Coriolanus", 67)
			.addRow("Coriolanus (Caius Marcius Coriolanus)", "Coriolanus", 189)
			.addRow("Junius Brutus", "Coriolanus", 91)
			.addRow("Lieutenant", "Coriolanus", 4)
			.addRow("Menenius Agrippa", "Coriolanus", 162)
			.addRow("Patrician", "Coriolanus", 3)
			.addRow("Roman", "Coriolanus", 10)
			.addRow("Sicinius Velutus", "Coriolanus", 117)
			.addRow("Titus Lartius", "Coriolanus", 23)
			.addRow("Tullus Aufidius", "Coriolanus", 45)
			.addRow("Valeria", "Coriolanus", 14)
			.addRow("Virgilia", "Coriolanus", 26)
			.addRow("Volsce", "Coriolanus", 9)
			.addRow("Volumnia", "Coriolanus", 57)
			.addRow("Young Coriolanus", "Coriolanus", 1)
			.addRow("Cymbeline", "Tragedies", null)
			.addRow("Arviragus", "Cymbeline", 46)
			.addRow("Belarius", "Cymbeline", 58)
			.addRow("Caius Lucius", "Cymbeline", 25)
			.addRow("Cloten", "Cymbeline", 77)
			.addRow("Cornelius (physician)", "Cymbeline", 13)
			.addRow("Cymbeline, King of Britain", "Cymbeline", 81)
			.addRow("Guiderius", "Cymbeline", 62)
			.addRow("Helen", "Cymbeline", 0)
			.addRow("Iachimo", "Cymbeline", 77)
			.addRow("Imogen", "Cymbeline", 118)
			.addRow("Jupiter", "Cymbeline", 1)
			.addRow("Philario", "Cymbeline", 14)
			.addRow("Pisanio", "Cymbeline", 58)
			.addRow("Posthumus Leonatus", "Cymbeline", 77)
			.addRow("Queen", "Cymbeline", 27)
			.addRow("Roman Captain", "Cymbeline", 4)
			.addRow("Sicilius Leonatus", "Cymbeline", 7)
			.addRow("The Tragedy of Hamlet, Prince of Denmark", "Tragedies", null)
			.addRow("Bernardo", "The Tragedy of Hamlet, Prince of Denmark", 19)
			.addRow("Claudius, King of Denmark", "The Tragedy of Hamlet, Prince of Denmark", 102)
			.addRow("Cornelius", "The Tragedy of Hamlet, Prince of Denmark", 1)
			.addRow("Father's Ghost", "The Tragedy of Hamlet, Prince of Denmark", 15)
			.addRow("Fortinbras", "The Tragedy of Hamlet, Prince of Denmark", 6)
			.addRow("Francisco ", "The Tragedy of Hamlet, Prince of Denmark", 8)
			.addRow("Gertrude", "The Tragedy of Hamlet, Prince of Denmark", 69)
			.addRow("Guildenstern", "The Tragedy of Hamlet, Prince of Denmark", 29)
			.addRow("Hamlet", "The Tragedy of Hamlet, Prince of Denmark", 358)
			.addRow("Horatio", "The Tragedy of Hamlet, Prince of Denmark", 109)
			.addRow("Laertes", "The Tragedy of Hamlet, Prince of Denmark", 62)
			.addRow("Lucianus", "The Tragedy of Hamlet, Prince of Denmark", 0)
			.addRow("Marcellus", "The Tragedy of Hamlet, Prince of Denmark", 37)
			.addRow("Ophelia", "The Tragedy of Hamlet, Prince of Denmark", 58)
			.addRow("Osric", "The Tragedy of Hamlet, Prince of Denmark", 25)
			.addRow("Polonius", "The Tragedy of Hamlet, Prince of Denmark", 86)
			.addRow("Reynaldo", "The Tragedy of Hamlet, Prince of Denmark", 13)
			.addRow("Rosencrantz", "The Tragedy of Hamlet, Prince of Denmark", 48)
			.addRow("Voltemand", "The Tragedy of Hamlet, Prince of Denmark", 1)
			.addRow("Julius Caesar", "Tragedies", null)
			.addRow("Antony (Marcus Antonius)", "Julius Caesar", 51)
			.addRow("Artemidorus of Cnidos", "Julius Caesar", 4)
			.addRow("Brutus (Marcus Brutus)", "Julius Caesar", 194)
			.addRow("Caesar (Julius Caesar)", "Julius Caesar", 42)
			.addRow("Calpurnia", "Julius Caesar", 6)
			.addRow("Casca", "Julius Caesar", 39)
			.addRow("Cassius", "Julius Caesar", 140)
			.addRow("Cicero", "Julius Caesar", 4)
			.addRow("Cinna", "Julius Caesar", 11)
			.addRow("Cinna the Poet", "Julius Caesar", 8)
			.addRow("Claudius", "Julius Caesar", 2)
			.addRow("Clitus", "Julius Caesar", 8)
			.addRow("Dardanius", "Julius Caesar", 3)
			.addRow("Decius Brutus", "Julius Caesar", 12)
			.addRow("Flavius", "Julius Caesar", 5)
			.addRow("Lepidus (Marcus Antonius Lepidus)", "Julius Caesar", 3)
			.addRow("Ligarius", "Julius Caesar", 5)
			.addRow("Lucilius", "Julius Caesar", 10)
			.addRow("Lucius", "Julius Caesar", 24)
			.addRow("Marullus", "Julius Caesar", 6)
			.addRow("Messala", "Julius Caesar", 20)
			.addRow("Metellus Cimber", "Julius Caesar", 5)
			.addRow("Octavius (Octavius Caesar)", "Julius Caesar", 19)
			.addRow("Pindarus", "Julius Caesar", 5)
			.addRow("Popilius (Popilius Lena)", "Julius Caesar", 2)
			.addRow("Portia (wife of Brutus)", "Julius Caesar", 16)
			.addRow("Publius", "Julius Caesar", 2)
			.addRow("Strato", "Julius Caesar", 4)
			.addRow("Tintinius", "Julius Caesar", 10)
			.addRow("Trebonius", "Julius Caesar", 4)
			.addRow("Varro", "Julius Caesar", 6)
			.addRow("Volumnius", "Julius Caesar", 3)
			.addRow("Young Cato", "Julius Caesar", 3)
			.addRow("King Lear", "Tragedies", null)
			.addRow("Cordelia", "King Lear", 31)
			.addRow("Curan", "King Lear", 4)
			.addRow("Duke of Albany", "King Lear", 58)
			.addRow("Duke of Burgundy", "King Lear", 5)
			.addRow("Duke of Cornwall", "King Lear", 53)
			.addRow("Earl of Gloucester", "King Lear", 118)
			.addRow("Earl of Kent", "King Lear", 127)
			.addRow("Edgar", "King Lear", 98)
			.addRow("Edmund", "King Lear", 79)
			.addRow("Goneril", "King Lear", 53)
			.addRow("King of France", "King Lear", 5)
			.addRow("Lear", "King Lear", 188)
			.addRow("Oswald", "King Lear", 38)
			.addRow("Regan", "King Lear", 73)
			.addRow("The Tragedy Of Macbeth", "Tragedies", null)
			.addRow("Angus", "The Tragedy Of Macbeth", 4)
			.addRow("Banquo", "The Tragedy Of Macbeth", 33)
			.addRow("Caithness", "The Tragedy Of Macbeth", 3)
			.addRow("Donalbain", "The Tragedy Of Macbeth", 3)
			.addRow("Duncan", "The Tragedy Of Macbeth", 18)
			.addRow("Fleance", "The Tragedy Of Macbeth", 2)
			.addRow("Hecate", "The Tragedy Of Macbeth", 2)
			.addRow("Lady Macbeth", "The Tragedy Of Macbeth", 59)
			.addRow("Lady Macduff", "The Tragedy Of Macbeth", 19)
			.addRow("Lennox", "The Tragedy Of Macbeth", 21)
			.addRow("Macbeth", "The Tragedy Of Macbeth", 146)
			.addRow("Macduff", "The Tragedy Of Macbeth", 59)
			.addRow("Malcolm", "The Tragedy Of Macbeth", 40)
			.addRow("Menteith", "The Tragedy Of Macbeth", 5)
			.addRow("Porter", "The Tragedy Of Macbeth", 4)
			.addRow("Ross", "The Tragedy Of Macbeth", 39)
			.addRow("Seyton", "The Tragedy Of Macbeth", 5)
			.addRow("Siward", "The Tragedy Of Macbeth", 11)
			.addRow("Son (Macduff's son)", "The Tragedy Of Macbeth", 14)
			.addRow("Young Siward", "The Tragedy Of Macbeth", 4)
			.addRow("The Tragedy Of Othello", "Tragedies", null)
			.addRow("Bianca (Mistress to Cassio)", "The Tragedy Of Othello", 15)
			.addRow("Brabantio", "The Tragedy Of Othello", 30)
			.addRow("Cassio", "The Tragedy Of Othello", 110)
			.addRow("Desdemona", "The Tragedy Of Othello", 165)
			.addRow("Duke of Venice", "The Tragedy Of Othello", 25)
			.addRow("Emilia", "The Tragedy Of Othello", 103)
			.addRow("Gratiano (Brother to Brabantio)", "The Tragedy Of Othello", 20)
			.addRow("Iago", "The Tragedy Of Othello", 272)
			.addRow("Lodovico", "The Tragedy Of Othello", 33)
			.addRow("Montano", "The Tragedy Of Othello", 24)
			.addRow("Othello", "The Tragedy Of Othello", 274)
			.addRow("Roderigo", "The Tragedy Of Othello", 59)
			.addRow("Romeo And Juliet", "Tragedies", null)
			.addRow("Abraham", "Romeo And Juliet", 5)
			.addRow("Balthasar (Servant to Romeo)", "Romeo And Juliet", 12)
			.addRow("Benvolio", "Romeo And Juliet", 64)
			.addRow("Capulet", "Romeo And Juliet", 51)
			.addRow("Friar John", "Romeo And Juliet", 4)
			.addRow("Friar Laurence", "Romeo And Juliet", 55)
			.addRow("Gregory", "Romeo And Juliet", 15)
			.addRow("Juliet", "Romeo And Juliet", 118)
			.addRow("Lady Capulet", "Romeo And Juliet", 45)
			.addRow("Lady Montague", "Romeo And Juliet", 2)
			.addRow("Mercutio", "Romeo And Juliet", 62)
			.addRow("Montague", "Romeo And Juliet", 10)
			.addRow("Paris", "Romeo And Juliet", 23)
			.addRow("Peter (Servant to Juliet's Nurse)", "Romeo And Juliet", 13)
			.addRow("Prince Escalus", "Romeo And Juliet", 16)
			.addRow("Romeo", "Romeo And Juliet", 163)
			.addRow("Sampson", "Romeo And Juliet", 20)
			.addRow("Tybalt", "Romeo And Juliet", 17);

		setHighlightOnMouseOver(true);
		setMaxDepth(1);
		setMaxPostDepth(2);
		setMinHighlightColor("#8c6bb1");
		setMidHighlightColor("#9ebcda");
		setMaxHighlightColor("#edf8fb");
		setMinColor("#009688");
		setMidColor("#f7f7f7");
		setMaxColor("#ee8100");
		setHeaderHeight(15);
		setShowScale(true);
		setUseWeightedAverageForAggregation(true);
	}
}
