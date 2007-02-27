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

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IParent;
import org.erlide.core.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * 
 * @author Vlad Dumitrescu
 */
public class ErlFunction extends ErlMember implements IErlFunction, IParent {

	private boolean fExported;

	private int fArity;

	/**
	 * @param parent
	 * @param name
	 */
	protected ErlFunction(ErlElement parent, String name, int arity) {
		super(parent, name);
		fArity = arity;
	}

	public IErlFunctionClause[] getClauses() {
		ArrayList<IErlFunctionClause> FC = new ArrayList<IErlFunctionClause>(
				fChildren.length);
		for (IErlElement el : fChildren) {
			if (el instanceof IErlFunctionClause) {
				FC.add((IErlFunctionClause) el);
			}
		}
		return FC.toArray(new IErlFunctionClause[FC.size()]);
	}

	public String getElementType() {
		return FUNCTION;
	}

	public int getArity() {
		return fArity;
	}

	public boolean isExported() {
		return fExported;
	}

	public void setArity(int i) {
		fArity = i;
	}

	public void setExported(boolean exported) {
		fExported = exported;
	}

	@Override
	public OtpErlangObject getParseTree() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getHoverHelp() {
		return super.getHoverHelp();
	}

	@Override
	public String toString() {
		return getElementName() + "/" + getArity();
	}

	public ErlangFunction getFunction() {
		return new ErlangFunction(getElementName(), getArity());
	}

	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = super.hashCode();
		result = PRIME * result + fArity;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
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