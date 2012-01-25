/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.launch.debug.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IDropToFrame;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangStackFrame extends ErlangDebugElement implements
        IStackFrame, IDropToFrame {

    private final String fModuleName;
    private final ErlangProcess fParent;
    private final int fLineNumber;
    List<ErlangVariable> bindings;
    int stackFrameNo;
    private String clauseHead;

    public ErlangStackFrame(final String moduleName,
            final ErlangProcess parent, final IDebugTarget target,
            int lineNumber, final ErlangFunction function,
            final OtpErlangList bindings, final int stackFrameNo) {
        super(target);
        fParent = parent;
        fModuleName = moduleName;
        this.stackFrameNo = stackFrameNo;
        final List<ErlangVariable> framesReversed = new ArrayList<ErlangVariable>(
                bindings.arity());
        for (final OtpErlangObject o : bindings) {
            if (o instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) o;
                final OtpErlangAtom nameA = (OtpErlangAtom) t.elementAt(0);
                final OtpErlangObject value = t.elementAt(1);
                framesReversed.add(new ErlangVariable(target,
                        nameA.atomValue(), false, value, parent, moduleName,
                        stackFrameNo));
            }
        }
        final List<ErlangVariable> frames = new ArrayList<ErlangVariable>(
                framesReversed.size());
        for (int i = framesReversed.size() - 1; i >= 0; --i) {
            frames.add(framesReversed.get(i));
        }
        this.bindings = frames;
        IErlModule module;
        try {
            module = ErlModelManager.getErlangModel().findModule(moduleName);
        } catch (final ErlModelException e) {
            ErlLogger.error(e);
            module = null;
        }
        clauseHead = null;
        if (module != null) {
            try {
                module.open(null);
                if (lineNumber != -1) {
                    final IErlElement e = module
                            .getElementAtLine(lineNumber - 1);
                    if (e instanceof IErlFunctionClause) {
                        final IErlFunctionClause clause = (IErlFunctionClause) e;
                        clauseHead = clause.getFunctionName()
                                + clause.getHead();
                    }
                } else if (function != null) {
                    final IErlFunction f = module.findFunction(function);
                    if (f != null) {
                        lineNumber = f.getLineStart() + 1;
                        clauseHead = f.getFunctionName() + f.getHead();
                    }
                }
            } catch (final ErlModelException e1) {
                ErlLogger.warn(e1);
            }
        }
        fLineNumber = lineNumber;
    }

    public String getModule() {
        return fModuleName;
    }

    @Override
    public IThread getThread() {
        return fParent;
    }

    @Override
    public IVariable[] getVariables() throws DebugException {
        return bindings.toArray(new IVariable[bindings.size()]);
    }

    @Override
    public boolean hasVariables() throws DebugException {
        return true;
    }

    @Override
    public int getLineNumber() throws DebugException {
        return fLineNumber;
    }

    @Override
    public int getCharStart() throws DebugException {
        return -1;
    }

    @Override
    public int getCharEnd() throws DebugException {
        return -1;
    }

    @Override
    public String getName() throws DebugException {
        return "<" + fModuleName + ">";
    }

    @Override
    public IRegisterGroup[] getRegisterGroups() throws DebugException {
        return null;
    }

    @Override
    public boolean hasRegisterGroups() throws DebugException {
        return false;
    }

    @Override
    public boolean canStepInto() {
        return fParent.canStepInto();
    }

    @Override
    public boolean canStepOver() {
        return fParent.canStepOver();
    }

    @Override
    public boolean canStepReturn() {
        return fParent.canStepReturn();
    }

    @Override
    public boolean isStepping() {
        return fParent.isStepping();
    }

    @Override
    public void stepInto() throws DebugException {
        fParent.stepInto();
    }

    @Override
    public void stepOver() throws DebugException {
        fParent.stepOver();
    }

    @Override
    public void stepReturn() throws DebugException {
        fParent.stepReturn();
    }

    @Override
    public boolean canResume() {
        return fParent.canResume();
    }

    @Override
    public boolean canSuspend() {
        return fParent.canSuspend();
    }

    @Override
    public boolean isSuspended() {
        return fParent.isSuspended();
    }

    @Override
    public void resume() throws DebugException {
        fParent.resume();
    }

    @Override
    public void suspend() throws DebugException {
        fParent.suspend();
    }

    @Override
    public boolean canTerminate() {
        return fParent.canTerminate();
    }

    @Override
    public boolean isTerminated() {
        return fParent.isTerminated();
    }

    @Override
    public void terminate() throws DebugException {
        fParent.terminate();
    }

    public String getClauseHead() {
        return clauseHead;
    }

    @Override
    public boolean canDropToFrame() {
        return true;
    }

    @Override
    public void dropToFrame() throws DebugException {
        fParent.dropToFrame(stackFrameNo);
    }

}
