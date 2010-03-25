/**
 *
 */
package org.erlide.core.builder;

import org.eclipse.core.resources.IResource;

public final class BuildResource {
	private final IResource resource;
	private final String output;

	// private final OtpErlangList compilerOptions;

	public BuildResource(IResource res, String out) {
		resource = res;
		output = out;
	}

	public BuildResource(IResource res) {
		resource = res;
		output = null;
	}

	public String getOutput() {
		return output;
	}

	public IResource getResource() {
		return resource;
	}

	// public OtpErlangList getCompilerOptions() {
	// return compilerOptions;
	// }

	@Override
	public String toString() {
		return resource.toString() + "#" + output;
	}
}