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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IPath;

public class LibraryEntry extends PathEntry {
    private List<SourceEntry> sources = new ArrayList<SourceEntry>();
    private List<IPath> includes = new ArrayList<IPath>();
    private final IPath output;
    private List<PathEntry> libraries = new ArrayList<PathEntry>();

    public LibraryEntry(final List<SourceEntry> sources,
            final List<IPath> includes, final IPath output,
            final List<PathEntry> libraries) {
        super(null);
        if (sources != null) {
            this.sources = sources;
        }
        if (includes != null) {
            this.includes = includes;
        }
        this.output = output;
        if (libraries != null) {
            this.libraries = libraries;
        }
    }

    public Collection<SourceEntry> getSources() {
        return Collections.unmodifiableCollection(sources);
    }

    public Collection<IPath> getIncludes() {
        return Collections.unmodifiableCollection(includes);
    }

    public IPath getOutput() {
        return output;
    }

    public Collection<PathEntry> getDependencies() {
        return Collections.unmodifiableCollection(libraries);
    }

}
