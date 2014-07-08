/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.core.internal;

import java.util.HashMap;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.util.ErlLogger;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage.GenFunReturnParameterName;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.ErlRange;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.Range;

import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Integration class of the generalise function refactoring
 *
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class GeneraliseFunctionRefactoring extends
        CostumWorkflowRefactoringWithPositionsSelection {

    /**
     * The state of the generalise function refactoring, after calling the first
     * RPC
     *
     * @author Gyorgy Orosz
     * @version %I%, %G%
     */
    public enum State {
        ok, error, multi_instance, unknown_side_effect, more_than_one_clause;
    }

    private final State state;
    private String errorMessage;
    private GenFunRefactoringMessage message;

    private boolean onlyInClause;
    private boolean sideEffect;

    /**
     * Constructor
     *
     * @param state
     *            state of the refactoring
     * @param text
     *            error message in case of wrong input
     */
    public GeneraliseFunctionRefactoring(final State state, final String text) {
        this.state = state;
        errorMessage = text;

    }

    /**
     * Constructor
     *
     * @param state
     *            state of the refactoring
     * @param message
     *            RPC message from Wrangler
     */
    public GeneraliseFunctionRefactoring(final State state,
            final GenFunRefactoringMessage message) {
        this.state = state;
        this.message = message;
    }

    /**
     * Constructor
     *
     * @param state
     *            state of the refactoring
     * @param message
     *            RPC message from Wrangler
     * @param onlyInClause
     *            user input
     */
    public GeneraliseFunctionRefactoring(final State state,
            final GenFunRefactoringMessage message, final boolean onlyInClause) {
        this(state, message);
        this.onlyInClause = onlyInClause;
    }

    /**
     * Constructor
     *
     * @param state
     *            state of the refactoring
     * @param message
     *            RPC message from Wrangler
     * @param onlyInClause
     *            user input
     * @param sideEffect
     *            user input
     */
    public GeneraliseFunctionRefactoring(final State state,
            final GenFunRefactoringMessage message, final boolean onlyInClause,
            final boolean sideEffect) {
        this(state, message, onlyInClause);
        this.sideEffect = sideEffect;

    }

    /*
     * public GeneraliseFunctionRefactoring(State state) { }
     */

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        if (state == State.error) {
            return RefactoringStatus.createFatalErrorStatus(errorMessage);
        } else if (state == State.multi_instance) {
            positions = createPositionList((OtpErlangList) message.getParameters().get(
                    GenFunReturnParameterName.dupsInFun));
        } else if (state == State.more_than_one_clause) {
            if (onlyInClause) {
                positions = createPositionList((OtpErlangList) message.getParameters()
                        .get(GenFunReturnParameterName.dupsInClause));
            } else {
                positions = createPositionList((OtpErlangList) message.getParameters()
                        .get(GenFunReturnParameterName.dupsInFun));
            }
        } else if (state == State.unknown_side_effect) {
            if (onlyInClause) {
                positions = createPositionList((OtpErlangList) message.getParameters()
                        .get(GenFunReturnParameterName.dupsInClause));
            } else {
                positions = createPositionList((OtpErlangList) message.getParameters()
                        .get(GenFunReturnParameterName.dupsInFun));
            }
        }
        return new RefactoringStatus();
    }

    /**
     * Returns IErlRange, OtpErlangTuple pairs, which are represents the same
     * position in a module
     *
     * @param thePositions
     * @return
     */
    protected HashMap<IErlRange, OtpErlangTuple> createPositionList(
            final OtpErlangList thePositions) {
        try {
            final HashMap<IErlRange, OtpErlangTuple> ret = new HashMap<IErlRange, OtpErlangTuple>();
            final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                    .getWranglerSelection();
            for (final OtpErlangObject o : thePositions) {
                // {{sl, sc}, {el,ec}}
                final OtpErlangTuple pos = (OtpErlangTuple) o;
                ret.put(new ErlRange(new Range(pos), sel.getDocument()), pos);
            }

            return ret;
        } catch (final OtpErlangException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    @Override
    public String getName() {
        return "Generalise function definition";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection sel) {

        if (state == State.ok) {
            return message;
        } else if (state == State.error) {
            return null;
        } else {
            final HashMap<GenFunReturnParameterName, OtpErlangObject> p = message
                    .getParameters();
            OtpErlangObject sideEffectPar, parName, funName, arity, funDefPos, exp, logCmd;
            sideEffectPar = p.get(GenFunReturnParameterName.sideEffect);
            parName = p.get(GenFunReturnParameterName.parName);
            funName = p.get(GenFunReturnParameterName.funName);
            arity = p.get(GenFunReturnParameterName.arity);
            funDefPos = p.get(GenFunReturnParameterName.funDefPos);
            exp = p.get(GenFunReturnParameterName.exp);
            logCmd = p.get(GenFunReturnParameterName.logCmd);
            if (state == State.multi_instance) {
                return WranglerBackendManager.getRefactoringBackend().call(
                        "gen_fun_1_eclipse", "xsxxxxxxxix", sideEffectPar,
                        sel.getFilePath(), parName, funName, arity, funDefPos, exp,
                        getSelectedPos(), sel.getSearchPath(),
                        GlobalParameters.getTabWidth(), logCmd);
            } else if (state == State.unknown_side_effect) {
                if (onlyInClause) {
                    return WranglerBackendManager.getRefactoringBackend().call(
                            "gen_fun_clause_eclipse", "sxxxxxixxx", sel.getFilePath(),
                            parName, funName, arity, funDefPos, exp,
                            GlobalParameters.getTabWidth(),
                            new OtpErlangBoolean(sideEffect), getSelectedPos(), logCmd);
                }
                return WranglerBackendManager.getRefactoringBackend().call(
                        "gen_fun_1_eclipse", "xsxxxxxxxix",
                        new OtpErlangBoolean(sideEffect), sel.getFilePath(), parName,
                        funName, arity, funDefPos, exp, getSelectedPos(),
                        sel.getSearchPath(), GlobalParameters.getTabWidth(), logCmd);
            } else if (state == State.more_than_one_clause) {
                if (onlyInClause) {
                    return WranglerBackendManager.getRefactoringBackend().call(
                            "gen_fun_clause_eclipse", "sxxxxxixxx", sel.getFilePath(),
                            parName, funName, arity, funDefPos, exp,
                            GlobalParameters.getTabWidth(), sideEffectPar,
                            getSelectedPos(), logCmd);
                }
                return WranglerBackendManager.getRefactoringBackend().call(
                        "gen_fun_1_eclipse", "xsxxxxxxxix", sideEffectPar,
                        sel.getFilePath(), parName, funName, arity, funDefPos, exp,
                        getSelectedPos(), sel.getSearchPath(),
                        GlobalParameters.getTabWidth(), logCmd);
            }
        }

        return null;

    }

    @Override
    public RefactoringStatus checkFinalConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        final IRefactoringRpcMessage theMessage = run(sel);
        if (theMessage.isSuccessful()) {
            changedFiles = theMessage.getRefactoringChangeset();
            return new RefactoringStatus();
        }
        return RefactoringStatus.createFatalErrorStatus(theMessage.getMessageString());
    }

    @Override
    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return new RefactoringWorkflowController(shell) {
            @Override
            public void doRefactoring() {
            }
        };
    }

    @Override
    public IRefactoringRpcMessage runAlternative(final IErlSelection selection) {
        return null;
    }

}
