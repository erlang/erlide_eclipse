package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;

public class ErlExternalReferencedModule extends ErlModuleWithoutResource
        implements IErlElement {

    protected ErlExternalReferencedModule(final IErlElement parent,
            final String nameWithExt, final String initialText,
            final String path) {
        super(parent, nameWithExt, initialText, path);
    }

}
