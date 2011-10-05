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
            positions = createPositionList((OtpErlangList) message
                    .getParameters().get(GenFunReturnParameterName.dupsInFun));
        } else if (state == State.more_than_one_clause) {
            if (onlyInClause) {
                positions = createPositionList((OtpErlangList) message
                        .getParameters().get(
                                GenFunReturnParameterName.dupsInClause));
            } else {
                positions = createPositionList((OtpErlangList) message
                        .getParameters().get(
                                GenFunReturnParameterName.dupsInFun));
            }
        } else if (state == State.unknown_side_effect) {
            if (onlyInClause) {
                positions = createPositionList((OtpErlangList) message
                        .getParameters().get(
                                GenFunReturnParameterName.dupsInClause));
            } else {
                positions = createPositionList((OtpErlangList) message
                        .getParameters().get(
                                GenFunReturnParameterName.dupsInFun));
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
            e.printStackTrace();
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
                        sel.getFilePath(), parName, funName, arity, funDefPos,
                        exp, getSelectedPos(), sel.getSearchPath(),
                        GlobalParameters.getTabWidth(), logCmd);
            } else if (state == State.unknown_side_effect) {
                if (onlyInClause) {
                    return WranglerBackendManager.getRefactoringBackend().call(
                            "gen_fun_clause_eclipse", "sxxxxxixxx",
                            sel.getFilePath(), parName, funName, arity,
                            funDefPos, exp, GlobalParameters.getTabWidth(),
                            new OtpErlangBoolean(sideEffect), getSelectedPos(),
                            logCmd);
                } else {
                    return WranglerBackendManager.getRefactoringBackend().call(
                            "gen_fun_1_eclipse", "xsxxxxxxxix",
                            new OtpErlangBoolean(sideEffect),
                            sel.getFilePath(), parName, funName, arity,
                            funDefPos, exp, getSelectedPos(),
                            sel.getSearchPath(),
                            GlobalParameters.getTabWidth(), logCmd);
                }
            } else if (state == State.more_than_one_clause) {
                if (onlyInClause) {
                    return WranglerBackendManager.getRefactoringBackend().call(
                            "gen_fun_clause_eclipse", "sxxxxxixxx",
                            sel.getFilePath(), parName, funName, arity,
                            funDefPos, exp, GlobalParameters.getTabWidth(),
                            sideEffectPar, getSelectedPos(), logCmd);
                } else {
                    return WranglerBackendManager.getRefactoringBackend().call(
                            "gen_fun_1_eclipse", "xsxxxxxxxix", sideEffectPar,
                            sel.getFilePath(), parName, funName, arity,
                            funDefPos, exp, getSelectedPos(),
                            sel.getSearchPath(),
                            GlobalParameters.getTabWidth(), logCmd);
                }
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
        } else {
            return RefactoringStatus.createFatalErrorStatus(theMessage
                    .getMessageString());
        }
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

    /*
     * protected Map<String, OtpErlangObject> parameters = null; protected
     * boolean hasSideEffect; protected RefactoringStatus status = null;
     * protected boolean doFirstCase;
     * 
     * @Override public RefactoringStatus
     * checkInitialConditions(IProgressMonitor pm) throws CoreException,
     * OperationCanceledException { IErlSelection sel =
     * GlobalParameters.getWranglerSelection(); if (sel instanceof
     * IErlMemberSelection) { SelectionKind kind = sel.getKind(); if (kind ==
     * SelectionKind.FUNCTION_CLAUSE || kind == SelectionKind.FUNCTION) return
     * new RefactoringStatus(); } // TODO: testing return RefactoringStatus
     * .createFatalErrorStatus("Please select an expression!"); }
     * 
     * @Override public RefactoringStatus checkFinalConditions(IProgressMonitor
     * pm) throws CoreException, OperationCanceledException {
     * IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
     * .getWranglerSelection(); if (status != null) { return status; } else if
     * (parameters != null) { IRefactoringRpcMessage msg; if (doFirstCase) { msg
     * = run(sel); } else { msg = runAlternative(sel); }
     * 
     * if (msg.isSuccessful()) { changedFiles = msg.getRefactoringChangeset();
     * return new RefactoringStatus(); } else { return
     * RefactoringStatus.createFatalErrorStatus(msg .getMessageString()); } }
     * return new RefactoringStatus();
     * 
     * }
     * 
     * protected HashMap<IErlRange, OtpErlangTuple> parsePositions(
     * OtpErlangList positions, IDocument doc) { HashMap<IErlRange,
     * OtpErlangTuple> ret = new HashMap<IErlRange, OtpErlangTuple>();
     * 
     * OtpErlangObject[] positionArray = positions.elements(); for
     * (OtpErlangObject o : positionArray) { try { ret.put(new ErlRange(new
     * Range((OtpErlangTuple) o), doc), (OtpErlangTuple) o); } catch
     * (OtpErlangRangeException e) { e.printStackTrace(); } }
     * 
     * return ret; }
     * 
     * @Override public String getName() { return "Generalise function"; }
     * 
     * protected GenFunRefactoringMessage preRun(IErlMemberSelection sel) {
     * return (GenFunRefactoringMessage) WranglerBackendManager
     * .getRefactoringBackend().callWithParser( new GenFunRefactoringMessage(),
     * "generalise_eclipse", "sxxsxi", sel.getFilePath(),
     * sel.getSelectionRange().getStartPos(),
     * sel.getSelectionRange().getEndPos(), userInput, sel.getSearchPath(),
     * GlobalParameters.getTabWidth());
     * 
     * }
     * 
     * @Override public IRefactoringRpcMessage run(IErlSelection selection) {
     * IErlMemberSelection sel = (IErlMemberSelection) selection; return
     * WranglerBackendManager.getRefactoringBackend().call( "gen_fun_1_eclipse",
     * "xsxxxxxix", new OtpErlangBoolean(this.hasSideEffect), sel.getFilePath(),
     * parameters.get("funName"), parameters.get("arity"),
     * parameters.get("funDefPos"), parameters.get("exp"), getSelectedPos(),
     * sel.getSearchPath(), GlobalParameters.getTabWidth(),
     * parameters.get("logCmd")); }
     * 
     * @Override public IRefactoringRpcMessage runAlternative(IErlSelection
     * selection) { IErlMemberSelection sel = (IErlMemberSelection) selection;
     * return WranglerBackendManager.getRefactoringBackend().call(
     * "gen_fun_eclipse", "sxxxxxxx", sel.getFilePath(),
     * parameters.get("parName"), parameters.get("funName"),
     * parameters.get("arity"), parameters.get("funDefPos"),
     * parameters.get("exp"), GlobalParameters.getTabWidth(), new
     * OtpErlangBoolean(hasSideEffect), getSelectedPos()); }
     * 
     * * public void setHasSideEffect(boolean b) { this.hasSideEffect = b; }
     * 
     * 
     * 
     * public boolean isUserInteractionNeeded() { IErlMemberSelection sel =
     * (IErlMemberSelection) GlobalParameters .getWranglerSelection();
     * SideEffectRefactoringMessage msg = trySideEffect(sel); syntaxinfo = null;
     * if (msg.isSuccessful()) { this.changedFiles =
     * msg.getRefactoringChangeset(); return false; } else if
     * (msg.isUserInteractionNeeded()) { syntaxinfo = msg.getSyntaxInfo();
     * return true; } else { this.status =
     * RefactoringStatus.createFatalErrorStatus(msg .getMessageString()); return
     * false; }
     * 
     * }
     * 
     * 
     * @Override public RefactoringWorkflowController
     * getWorkflowController(Shell shell) { return new
     * RefactoringWorkflowController(shell) { boolean controlPageOrder = false;
     * 
     * @Override public void doRefactoring() { IErlMemberSelection selection =
     * (IErlMemberSelection) GlobalParameters .getWranglerSelection();
     * GenFunRefactoringMessage msg = preRun(selection); if
     * (msg.getRefactoringState() == RefactoringState.ERROR) { status =
     * RefactoringStatus.createFatalErrorStatus(msg .getMessageString()); } else
     * if (msg.getRefactoringState() == RefactoringState.OK) { controlPageOrder
     * = true; changedFiles = msg.getRefactoringChangeset(); status = new
     * RefactoringStatus(); } else if (msg.getRefactoringState() ==
     * RefactoringState.UNKNOWN_SIDE_EFFECT) { hasSideEffect = ask("Question",
     * "Does the selected expression have side effect?"); OtpErlangInt
     * noOfClauses = (OtpErlangInt) msg .getParameters().get("noOfClauses"); try
     * { if (noOfClauses.intValue() > 1) chooseFromSelectionTypes(msg,
     * selection); else doSelectionDupsinFun(msg, selection); } catch
     * (OtpErlangRangeException e) { status = RefactoringStatus
     * .createFatalErrorStatus("Internal error!\n Please report it!"); } } else
     * if (msg.getRefactoringState() == RefactoringState.MULTI_INSTANCES) {
     * doSelectionDupsinFun(msg, selection);
     * 
     * } else if (msg.getRefactoringState() ==
     * RefactoringState.MORE_THAN_ONE_CLAUSE) { chooseFromSelectionTypes(msg,
     * selection); }
     * 
     * }
     * 
     * protected void doSelectionDupsinFun(GenFunRefactoringMessage msg,
     * IErlMemberSelection sel) { doFirstCase = true; positions =
     * parsePositions((OtpErlangList) parameters .get("dupsInFun"),
     * sel.getDocument()); }
     * 
     * protected void chooseFromSelectionTypes( GenFunRefactoringMessage msg,
     * IErlMemberSelection sel) { if (ask( "Question",
     * "The function selected has multiple clauses, would you like to generalise the function clause selected only?"
     * )) { doSelectionDupsinFun(msg, sel); } else {
     * doSelectionDupsinClause(msg, sel); }
     * 
     * }
     * 
     * protected void doSelectionDupsinClause( GenFunRefactoringMessage msg,
     * IErlMemberSelection sel) { doFirstCase = false; positions =
     * parsePositions((OtpErlangList) parameters .get("dupsInClause"),
     * sel.getDocument()); }
     * 
     * }; }
     */

}
