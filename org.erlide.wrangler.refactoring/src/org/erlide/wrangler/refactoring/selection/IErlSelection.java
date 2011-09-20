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
package org.erlide.wrangler.refactoring.selection;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ISelection;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;

import com.ericsson.otp.erlang.OtpErlangList;

/**
 * Interface for Erlang selection
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IErlSelection extends ISelection {
    /**
     * Denotes the kind of a selection in the editor plain.
     * 
     * We can differentiate: module, functino, function clause, expression,
     * variable
     * 
     * @author Gyorgy Orosz
     * @version %I%, %G%
     */
    public enum SelectionKind {
        MODULE, FUNCTION, FUNCTION_CLAUSE, EXPRESSION, VARIABLE;
    }

    /**
     * Get the sort of the selection
     * 
     * Erlide interface is used to get the kind.
     * 
     * @return kind of the selection
     */
    public SelectionKind getKind();

    /**
     * Get detailed kind of the selection.
     * 
     * Wrangler interface is used to get the kind
     * 
     * @return kind of the selection
     */
    public SelectionKind getDetailedKind();

    /**
     * Get the actual selection's file path.
     * 
     * @return file path string
     */
    public String getFilePath();

    /**
     * Get the actual file
     * 
     * @return Ifile object
     */
    public IFile getFile();

    /**
     * Get the actual selection's module.
     * 
     * @return
     */
    public IErlModule getErlModule();

    /**
     * Get the actual's selections project's search path.
     * 
     * @return list of directories
     */
    public OtpErlangList getSearchPath();

    /**
     * Get the corresponding Erlang element
     * 
     * @return IErlElement object
     */
    public IErlElement getErlElement();

}
