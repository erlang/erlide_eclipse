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

import org.erlide.core.erlang.ErlangCore;

public class WranglerBackendManager {
	static WranglerRefactoringBackend refactoringBackend = null;
	static WranglerSyntaxBackend syntaxBackend = null;

	public static WranglerRefactoringBackend getRefactoringBackend() {
		if (refactoringBackend == null)
			refactoringBackend = new WranglerRefactoringBackend(ErlangCore
					.getBackendManager().getIdeBackend());
		return refactoringBackend;
	}

	public static WranglerSyntaxBackend getSyntaxBackend() {
		if (syntaxBackend == null)
			syntaxBackend = new WranglerSyntaxBackend(ErlangCore
					.getBackendManager().getIdeBackend());
		return syntaxBackend;
	}
}
