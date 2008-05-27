/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IParent;
import org.erlide.core.util.ErlangFunction;

/**
 * 
 * @author Vlad Dumitrescu
 */
public class ErlFunction extends ErlMember implements IErlFunction, IParent {

	private boolean fExported;

	private int fArity;

	private final String parameters;

	/**
	 * @param parent
	 * @param name
	 */
	protected ErlFunction(final ErlElement parent, final String name,
			final int arity, final String parameters) {
		super(parent, name);
		fArity = arity;
		this.parameters = parameters;
	}

	public List<IErlFunctionClause> getClauses() {
		final ArrayList<IErlFunctionClause> fc = new ArrayList<IErlFunctionClause>();
		for (final IErlElement el : fChildren) {
			if (el instanceof IErlFunctionClause) {
				fc.add((IErlFunctionClause) el);
			}
		}
		return fc;
	}

	public Kind getKind() {
		return Kind.FUNCTION;
	}

	public int getArity() {
		return fArity;
	}

	public boolean isExported() {
		return fExported;
	}

	public void setArity(final int i) {
		fArity = i;
	}

	public void setExported(final boolean exported) {
		fExported = exported;
	}

	@Override
	public String toString() {
		if (parameters != null && parameters != "") {
			return getName() + parameters;
		} else {
			return getName() + "/" + getArity();
		}
	}

	public ErlangFunction getFunction() {
		return new ErlangFunction(getName(), getArity());
	}

	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = super.hashCode();
		result = PRIME * result + fArity;
		return result;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final ErlFunction other = (ErlFunction) obj;
		if (fArity != other.fArity) {
			return false;
		}
		return true;
	}
}