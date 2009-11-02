package org.erlide.core.text;

import org.eclipse.core.resources.IResource;
import org.erlide.core.erlang.IErlModule;

public class ErlangToolkit {

	public static String createScannerModuleName(final IErlModule module) {
		final IResource res = module.getResource();
		String resName;
		if (res != null) {
			resName = "mod" + res.getFullPath().toPortableString().hashCode()
					+ "_" + res.getName();
		} else {
			// This is not used more than temporarily, so it's OK to have
			// a name that's temporary, as long as it's unique
			resName = "mod" + module.hashCode() + "_";
		}
		return resName;
	}

}
