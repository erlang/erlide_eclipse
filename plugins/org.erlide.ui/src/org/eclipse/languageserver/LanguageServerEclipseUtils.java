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

import java.net.URI;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.TextDocumentPositionParams;

/**
 * Some utility methods to convert between Eclipse and LS-API types
 */
public class LanguageServerEclipseUtils {

    public static Position toPosition(final int offset, final IDocument document)
            throws BadLocationException {
        final Position res = new Position();
        res.setLine(document.getLineOfOffset(offset));
        res.setCharacter(
                offset - document.getLineInformationOfOffset(offset).getOffset());
        return res;
    }

    public static int toOffset(final Position position, final IDocument document)
            throws BadLocationException {
        return document.getLineInformation(position.getLine()).getOffset()
                + position.getCharacter();
    }

    public static TextDocumentPositionParams toTextDocumentPosistionParams(
            final URI fileUri, final int offset, final IDocument document)
            throws BadLocationException {
        final Position start = toPosition(offset, document);
        final TextDocumentPositionParams param = new TextDocumentPositionParams();
        param.setPosition(start);
        final TextDocumentIdentifier id = new TextDocumentIdentifier(fileUri.toString());
        param.setTextDocument(id);
        return param;
    }

    public static int toEclipseMarkerSeverity(final DiagnosticSeverity lspSeverity) {
        switch (lspSeverity) {
        case Error:
            return IMarker.SEVERITY_ERROR;
        case Warning:
            return IMarker.SEVERITY_WARNING;
        case Information:
            return IMarker.SEVERITY_INFO;
        default:
            break;
        }
        return IMarker.SEVERITY_INFO;
    }

    public static IResource findResourceFor(String uri) {
        String myUri = uri.replace("file:///", "file:/");
        myUri = myUri.replace("file://", "file:/");
        IProject project = null;
        for (final IProject aProject : ResourcesPlugin.getWorkspace().getRoot()
                .getProjects()) {
            if (myUri.startsWith(aProject.getLocationURI().toString())
                    && (project == null || project.getLocation().segmentCount() < aProject
                            .getLocation().segmentCount())) {
                project = aProject;
            }
        }
        if (project == null) {
            return null;
        }
        final IResource resource = project.getFile(
                new Path(myUri.substring(project.getLocationURI().toString().length())));
        if (!resource.exists()) {
            // resource.refresh ?
        }
        return resource;
    }

}
