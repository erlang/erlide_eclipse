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

import org.eclipse.jface.text.IDocument;
import org.erlide.wrangler.refactoring.util.IErlRange;

/**
 * Selection which represents an Erlang member.
 * 
 * (Erl module is not an erlang member but could be a selection)
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IErlMemberSelection extends IErlSelection {
    /**
     * Get the selection range of the member
     * 
     * @return selection range
     */
    public IErlRange getSelectionRange();

    /**
     * Get the member range
     * 
     * @return range
     */
    public IErlRange getMemberRange();

    /**
     * Get the document which conatins the member
     * 
     * @return document
     */
    public IDocument getDocument();

}
