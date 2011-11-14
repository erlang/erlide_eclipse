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
package org.erlide.wrangler.refactoring.duplicatedcode.core;

import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.WranglerRefactoringBackend;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Expression search refactoring runner
 * 
 * @author Gyorgy Orosz
 * 
 */
public class ExpressionSearchAction extends AbstractDuplicatesSearcherAction {

    @Override
    protected IResultParser callRefactoring()
            throws WranglerRpcParsingException {
        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();
        final WranglerRefactoringBackend backend = WranglerBackendManager
                .getRefactoringBackend();
        final RpcResult result = backend.callWithoutParser(
                WranglerRefactoringBackend.UNLIMITED_TIMEOUT,
                "expr_search_eclipse", "sxxi", sel.getFilePath(), sel
                        .getSelectionRange().getStartPos(), sel
                        .getSelectionRange().getEndPos(), GlobalParameters
                        .getTabWidth());
        if (result.isOk()) {
            return new ExpressionSearchParser(result.getValue());
        } else {
            throw new WranglerRpcParsingException("RPC error");
        }
    }

    @Override
    protected boolean getUserInput() {
        return true;
    }

}
