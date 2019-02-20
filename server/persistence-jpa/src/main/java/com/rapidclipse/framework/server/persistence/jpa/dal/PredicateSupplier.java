
package com.rapidclipse.framework.server.persistence.jpa.dal;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;


/**
 * @author XDEV Software
 *
 */
public interface PredicateSupplier
{
	public <T> Predicate getPredicate(
		CriteriaQuery<?> criteriaQuery,
		Root<T> root,
		CriteriaBuilder builder,
		T entity);
}
