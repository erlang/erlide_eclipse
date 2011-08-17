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

import java.util.ArrayList;

/**
 * Interface for handling RpcMessages which contains information about a
 * Wrangler refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IRefactoringRpcMessage extends IRpcMessage {
    /**
     * Successful refactorings contains source file modification, which are
     * represented with filename-source code pairs
     * 
     * @return changed files list
     */
    public ArrayList<ChangedFile> getRefactoringChangeset();

}
