/**
 *
 */
package org.erlide.core.builder;

import org.eclipse.core.resources.IResource;

import com.ericsson.otp.erlang.OtpErlangList;

final class BuildResource {
	private final IResource resource;
	private final String output;
	private final OtpErlangList compilerOptions;

	public BuildResource(IResource resource2, String out, OtpErlangList options) {
		resource = resource2;
		output = out;
		compilerOptions = options;
	}

	public String getOutput() {
		return output;
	}

	public IResource getResource() {
		return resource;
	}

	public OtpErlangList getCompilerOptions() {
		return compilerOptions;
	}
}