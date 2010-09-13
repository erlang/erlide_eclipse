/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.preferences;

import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;

public final class SourceEntry extends PathEntry {
    private final IPath output;

    public SourceEntry(final IPath directory, final IPath output,
            final Map<String, String> compilerOptions) {
        super(directory);
        Assert.isLegal(directory != null,
                "SourceLocation requires a non-null directory");
        this.output = output;
    }

    public IPath getOutput() {
        return output;
    }

}
