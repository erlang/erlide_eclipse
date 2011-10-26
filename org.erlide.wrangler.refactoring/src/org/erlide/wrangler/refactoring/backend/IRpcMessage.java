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
package org.erlide.wrangler.refactoring.backend;

import org.erlide.jinterface.rpc.RpcResult;

/**
 * Interface for handling RpcResultImpl objects of the Erlide's backend, which
 * contains information about a RPC with Wrangler
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IRpcMessage {
    /**
     * Returns true if the wrangler operation was successful.
     * 
     * @return true if the wrangler operation node was successful
     */
    public boolean isSuccessful();

    /**
     * If Wrangler could not perform the operation, it returns with an error or
     * warning message. It is returned by this function.
     * 
     * @return error message from wrangler
     */
    public String getMessageString();

    /**
     * Returns with the state of the refactoring
     * 
     * @return RefactoringState
     */
    public RefactoringState getRefactoringState();

    /**
     * Parses an RpcResultImpl object.
     * 
     * @param result
     *            result of an RPC
     */
    public void parse(RpcResult result);

}
