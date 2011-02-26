/**
 *
 */
package org.erlide.core.services.builder;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;

public final class BuildResource {
    private final IResource resource;
    private final String output;

    // private final OtpErlangList compilerOptions;

    public BuildResource(final IResource res, final String out) {
        resource = res;
        output = out;
    }

    public BuildResource(final IResource res, final IContainer out) {
        resource = res;
        output = out.getRawLocation().toString();
    }

    public BuildResource(final IResource res) {
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
