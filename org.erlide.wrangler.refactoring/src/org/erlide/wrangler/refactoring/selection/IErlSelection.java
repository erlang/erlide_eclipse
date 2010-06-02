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
import org.erlide.core.erlang.IErlElement;

import com.ericsson.otp.erlang.OtpErlangList;

public interface IErlSelection extends ISelection {
	public enum SelectionKind {
		MODULE, FUNCTION, FUNCTION_CLAUSE, EXPRESSION, VARIABLE;
	}

	public SelectionKind getKind();

	public SelectionKind getDetailedKind();

	public String getFilePath();

	public IFile getFile();

	public OtpErlangList getSearchPath();

	public IErlElement getErlElement();

}
