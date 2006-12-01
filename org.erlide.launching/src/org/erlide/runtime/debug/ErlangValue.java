/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

public class ErlangValue extends ErlangDebugElement implements IValue {

	public ErlangValue(IDebugTarget target) {
		super(target);
		// TODO Auto-generated constructor stub
	}

	public String getReferenceTypeName() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public String getValueString() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean isAllocated() throws DebugException {
		// TODO Auto-generated method stub
		return false;
	}

	public IVariable[] getVariables() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean hasVariables() throws DebugException {
		// TODO Auto-generated method stub
		return false;
	}

}
