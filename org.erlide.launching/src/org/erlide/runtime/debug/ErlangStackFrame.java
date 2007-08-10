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
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;

public class ErlangStackFrame extends ErlangDebugElement implements IStackFrame {

	private String fModule;

	private IThread fParent;

	public ErlangStackFrame(String module, IThread parent, IDebugTarget target) {
		super(target);
		fParent = parent;
		fModule = module;
	}

	public String getModule() {
		return fModule;
	}

	public IThread getThread() {
		return fParent;
	}

	public IVariable[] getVariables() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean hasVariables() throws DebugException {
		// TODO Auto-generated method stub
		return false;
	}

	public int getLineNumber() throws DebugException {
		// TODO Auto-generated method stub
		return 0;
	}

	public int getCharStart() throws DebugException {
		// TODO Auto-generated method stub
		return 0;
	}

	public int getCharEnd() throws DebugException {
		// TODO Auto-generated method stub
		return 0;
	}

	public String getName() throws DebugException {
		// TODO Auto-generated method stub
		return "n:" + fModule;
	}

	public IRegisterGroup[] getRegisterGroups() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean hasRegisterGroups() throws DebugException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getModelIdentifier() {
		return IErlDebugConstants.ID_ERLANG_DEBUG_MODEL;
	}

	@Override
	public IDebugTarget getDebugTarget() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ILaunch getLaunch() {
		// TODO Auto-generated method stub
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object getAdapter(Class adapter) {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean canStepInto() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canStepOver() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canStepReturn() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isStepping() {
		// TODO Auto-generated method stub
		return false;
	}

	public void stepInto() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void stepOver() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void stepReturn() throws DebugException {
		// TODO Auto-generated method stub

	}

	public boolean canResume() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canSuspend() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isSuspended() {
		// TODO Auto-generated method stub
		return false;
	}

	public void resume() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void suspend() throws DebugException {
		// TODO Auto-generated method stub

	}

	public boolean canTerminate() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isTerminated() {
		// TODO Auto-generated method stub
		return false;
	}

	public void terminate() throws DebugException {
		// TODO Auto-generated method stub

	}

}
