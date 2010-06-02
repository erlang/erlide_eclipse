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

public interface IErlMemberSelection extends IErlSelection {
	public IErlRange getSelectionRange();

	public IErlRange getMemberRange();

	public IDocument getDocument();

}
