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
