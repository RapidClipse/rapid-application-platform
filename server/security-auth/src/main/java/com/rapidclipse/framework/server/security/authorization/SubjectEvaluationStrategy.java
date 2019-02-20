
package com.rapidclipse.framework.server.security.authorization;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasEnabled;
import com.vaadin.flow.component.HasValue;


/**
 * Strategy used by {@link SubjectEvaluatingComponentExtension} to change a
 * component's state after a subject was evaluated.
 */
@FunctionalInterface
public interface SubjectEvaluationStrategy
{
	/**
	 * Default strategy which enabled or disabled a component depending on the
	 * evaluation's result.
	 */
	public final static SubjectEvaluationStrategy ENABLED   = (component, hasPermissions) -> {
																if(component instanceof HasEnabled)
																{
																	final HasEnabled e = (HasEnabled)component;
																	if(e.isEnabled() != hasPermissions)
																	{
																		e.setEnabled(
																			hasPermissions);
																	}
																}
															};
	
	/**
	 * Default strategy which shows or hides a component depending on the
	 * evaluation's result.
	 */
	public final static SubjectEvaluationStrategy VISIBLE   = (component, hasPermissions) -> {
																if(component
																	.isVisible() != hasPermissions)
																{
																	component.setVisible(
																		hasPermissions);
																}
															};
	
	/**
	 * Default strategy which sets read only mode for the component depending on
	 * the evaluation's result.
	 */
	public final static SubjectEvaluationStrategy READ_ONLY = (component, hasPermissions) -> {
																if(component instanceof HasValue)
																{
																	@SuppressWarnings("rawtypes")
																	final HasValue v = (HasValue)component;
																	if(v.isReadOnly() == hasPermissions)
																	{
																		v.setReadOnly(
																			!hasPermissions);
																	}
																}
															};
	
	/**
	 * Called after a subject was evaluated.
	 *
	 * @param component
	 *            the component to change
	 * @param hasPermissions
	 *            <code>true</code> if the subject had all necessary
	 *            permissions, <code>false</code> otherwise
	 */
	public void subjectEvaluated(Component component, boolean hasPermissions);
}
