/*******************************************************************************
 * Copyright (c) 2016 Red Hat Inc. and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *  Mickael Istria (Red Hat Inc.) - initial implementation
 *******************************************************************************/
package org.eclipse.languageserver;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.jface.text.IDocument;
import org.eclipse.lsp4j.services.LanguageServer;

/**
 * The entry-point to retrieve a Language Server for a given resource/project.
 * Deals with instantiations and caching of underlying
 * {@link ProjectSpecificLanguageServerWrapper}.
 *
 */
public class LanguageServiceAccessor {

    private static Map<IProject, Map<IContentType, ProjectSpecificLanguageServerWrapper>> projectServers = new HashMap<>();

    public static LanguageServer getLanguageServer(final IFile file,
            final IDocument document) throws IOException {
        final IProject project = file.getProject();
        if (!projectServers.containsKey(project)) {
            projectServers.put(project, new HashMap<>());
        }
        IContentType[] contentTypes = null;
        try (InputStream contents = file.getContents()) {
            contentTypes = Platform.getContentTypeManager().findContentTypesFor(contents,
                    file.getName()); // TODO consider using document as inputstream
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        ProjectSpecificLanguageServerWrapper wrapper = null;
        // 1st: search existing server for that file
        for (final IContentType contentType : contentTypes) {
            if (wrapper == null) {
                wrapper = projectServers.get(project).get(contentType);
            }
        }
        if (wrapper == null) {
            // try to create one for available content type
            for (final IContentType contentType : contentTypes) {
                if (wrapper == null) {
                    final StreamConnectionProvider connection = LSPStreamConnectionProviderRegistry
                            .getInstance().findProviderFor(contentType);
                    if (connection != null) {
                        wrapper = new ProjectSpecificLanguageServerWrapper(project,
                                contentType, connection);
                        projectServers.get(project).put(contentType, wrapper);
                    }
                }
            }
        }
        if (wrapper != null) {
            wrapper.connect(file, document);
            return wrapper.getServer();
        }
        return null;
    }
}
