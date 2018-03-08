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
import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.lsp4j.ClientCapabilities;
import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.TextDocumentContentChangeEvent;
import org.eclipse.lsp4j.TextDocumentItem;
import org.eclipse.lsp4j.VersionedTextDocumentIdentifier;
import org.eclipse.lsp4j.services.LanguageServer;
import org.erlide.server.ErlangLanguageServer;

/**
 * Wraps instantiation, initialization of project-specific instance of the
 * language server
 */
public class ProjectSpecificLanguageServerWrapper {

    private final class DocumentChangeListenenr implements IDocumentListener {
        private final URI fileURI;
        private int version = 2;
        private DidChangeTextDocumentParams change;

        public DocumentChangeListenenr(final URI fileURI) {
            this.fileURI = fileURI;
        }

        @Override
        public void documentChanged(final DocumentEvent event) {
            change.getContentChanges().get(0).setText(event.getDocument().get());
            server.getTextDocumentService().didChange(change);
            version++;
        }

        @Override
        public void documentAboutToBeChanged(final DocumentEvent event) {
            // try {
            change = new DidChangeTextDocumentParams();
            final VersionedTextDocumentIdentifier doc = new VersionedTextDocumentIdentifier();
            doc.setUri(fileURI.toString());
            doc.setVersion(version);
            change.setTextDocument(doc);
            final TextDocumentContentChangeEvent changeEvent = new TextDocumentContentChangeEvent();
            // RangeImpl range = new RangeImpl();
            // PositionImpl start =
            // LanguageServerEclipseUtils.toPosition(event.getOffset(),
            // event.getDocument());
            // range.setStart(start);
            // PositionImpl end = LanguageServerEclipseUtils.toPosition(event.getOffset()
            // + event.getLength(), event.getDocument());
            // range.setEnd(end);
            // changeEvent.setRange(range);
            // changeEvent.setRangeLength(event.getLength());
            changeEvent.setText(event.getDocument().get()); // TODO set to value after
                                                            // change
            change.setContentChanges(Arrays
                    .asList(new TextDocumentContentChangeEvent[] { changeEvent }));
            // } catch (BadLocationException ex) {
            // ex.printStackTrace(); // TODO
            // }
        }
    }

    protected static final String LS_DIAGNOSTIC_MARKER_TYPE = "org.eclipse.languageserver.diagnostic"; //$NON-NLS-1$

    private final StreamConnectionProvider lspStreamProvider;
    private ErlangLanguageServer server;
    private final IProject project;
    private final IContentType contentType;
    private Map<IPath, DocumentChangeListenenr> connectedFiles;

    public ProjectSpecificLanguageServerWrapper(final IProject project,
            final IContentType contentType, final StreamConnectionProvider connection) {
        this.project = project;
        this.contentType = contentType;
        lspStreamProvider = connection;
    }

    private void start() throws IOException {
        if (server != null) {
            return;
        }
        server = new ErlangLanguageServer();
//        server.onError(new Procedure2<String, Throwable>() {
//            @Override
//            public void apply(final String p1, final Throwable p2) {
//                System.err.println(p1);
//                p2.printStackTrace();
//            }
//        });
        lspStreamProvider.start();
//        server.connect(lspStreamProvider.getInputStream(),
//                lspStreamProvider.getOutputStream());
//        server.getProtocol().addErrorListener(new Procedure2<String, Throwable>() {
//            @Override
//            public void apply(final String p1, final Throwable p2) {
//                System.err.println("error: " + p1);
//            }
//        });
//        server.getProtocol()
//                .addIncomingMessageListener(new Procedure2<Message, String>() {
//                    @Override
//                    public void apply(final Message p1, final String p2) {
//                        System.err.println("IN: " + p1.getJsonrpc() + "\n" + p2);
//                    }
//                });
//        server.getProtocol()
//                .addOutgoingMessageListener(new Procedure2<Message, String>() {
//                    @Override
//                    public void apply(final Message p1, final String p2) {
//                        System.err.println("OUT: " + p1.getJsonrpc() + "\n" + p2);
//                    }
//                });
        // initialize
        final InitializeParams initParams = new InitializeParams();
        initParams.setRootPath(project.getLocation().toFile().getAbsolutePath());
        String name = "Eclipse IDE";
        if (Platform.getProduct() != null) {
            name = Platform.getProduct().getName();
        }
        initParams.setClientName(name);
        Integer.valueOf(java.lang.management.ManagementFactory.getRuntimeMXBean()
                .getName().split("@")[0]);
        initParams.setCapabilities(new ClientCapabilities());
        connectDiagnostics();
        final CompletableFuture<InitializeResult> result = server.initialize(initParams);
        try {
            final InitializeResult initializeResult = result.get();
        } catch (InterruptedException | ExecutionException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        connectedFiles = new HashMap<>();
    }

    private void connectDiagnostics() {
//        server.getTextDocumentService()
//                .onPublishDiagnostics(new Consumer<PublishDiagnosticsParams>() {
//                    @Override
//                    public void accept(final PublishDiagnosticsParams diagnostics) {
//                        try {
//                            // fix issue with file:/// vs file:/
//                            final String uri = diagnostics.getUri();
//                            IResource resource = LanguageServerEclipseUtils
//                                    .findResourceFor(uri);
//                            if (resource == null || !resource.exists()) {
//                                resource = project;
//                            }
//                            final Set<IMarker> remainingMarkers = new HashSet<>(
//                                    Arrays.asList(resource.findMarkers(
//                                            LS_DIAGNOSTIC_MARKER_TYPE, false,
//                                            IResource.DEPTH_ONE)));
//                            for (final Diagnostic diagnostic : diagnostics
//                                    .getDiagnostics()) {
//                                final IMarker associatedMarker = getExistingMarkerFor(
//                                        resource, diagnostic, remainingMarkers);
//                                if (associatedMarker == null) {
//                                    createMarkerForDiagnostic(resource, diagnostic);
//                                } else {
//                                    remainingMarkers.remove(associatedMarker);
//                                }
//                            }
//                            for (final IMarker marker : remainingMarkers) {
//                                marker.delete();
//                            }
//                        } catch (final CoreException ex) {
//                            ex.printStackTrace(); // TODO
//                        }
//                    }
//
//                    private void createMarkerForDiagnostic(final IResource resource,
//                            final Diagnostic diagnostic) {
//                        try {
//                            final IMarker marker = resource
//                                    .createMarker(LS_DIAGNOSTIC_MARKER_TYPE);
//                            marker.setAttribute(IMarker.MESSAGE, diagnostic.getMessage());
//                            marker.setAttribute(IMarker.SEVERITY,
//                                    LanguageServerEclipseUtils.toEclipseMarkerSeverity(
//                                            diagnostic.getSeverity())); // TODO mapping
//                                                                        // Eclipse <-> LS
//                                                                        // severity
//                            if (resource.getType() == IResource.FILE) {
//                                final IFile file = (IFile) resource;
//                                final IDocument document = FileBuffers
//                                        .getTextFileBufferManager()
//                                        .getTextFileBuffer(file.getFullPath(),
//                                                LocationKind.IFILE)
//                                        .getDocument();
//                                marker.setAttribute(IMarker.CHAR_START,
//                                        LanguageServerEclipseUtils.toOffset(
//                                                diagnostic.getRange().getStart(),
//                                                document));
//                                marker.setAttribute(IMarker.CHAR_END,
//                                        LanguageServerEclipseUtils.toOffset(
//                                                diagnostic.getRange().getEnd(),
//                                                document));
//                                marker.setAttribute(IMarker.LINE_NUMBER,
//                                        diagnostic.getRange().getStart().getLine());
//                            }
//                        } catch (final Exception ex) {
//                            ex.printStackTrace(); // TODO
//                        }
//                    }
//
//                    private IMarker getExistingMarkerFor(final IResource resource,
//                            final Diagnostic diagnostic,
//                            final Set<IMarker> remainingMarkers) {
//                        final ITextFileBuffer textFileBuffer = FileBuffers
//                                .getTextFileBufferManager().getTextFileBuffer(
//                                        resource.getFullPath(), LocationKind.IFILE);
//                        if (textFileBuffer == null) {
//                            return null;
//                        }
//                        final IDocument document = textFileBuffer.getDocument();
//                        for (final IMarker marker : remainingMarkers) {
//                            final int startOffset = marker
//                                    .getAttribute(IMarker.CHAR_START, -1);
//                            final int endOffset = marker.getAttribute(IMarker.CHAR_END,
//                                    -1);
//                            try {
//                                if (marker.getResource().getProjectRelativePath()
//                                        .toString().equals(diagnostic.getSource())
//                                        && LanguageServerEclipseUtils.toOffset(
//                                                diagnostic.getRange().getStart(),
//                                                document) == startOffset + 1
//                                        && LanguageServerEclipseUtils.toOffset(
//                                                diagnostic.getRange().getEnd(),
//                                                document) == endOffset + 1
//                                        && Objects.equal(
//                                                marker.getAttribute(IMarker.MESSAGE),
//                                                diagnostic.getMessage())) {
//                                    return marker;
//                                }
//                            } catch (final Exception e) {
//                                e.printStackTrace(); // TODO
//                            }
//                        }
//                        return null;
//                    }
//                });
    }

    private void stop() {
        lspStreamProvider.stop();
        server.shutdown();
        server = null;
    }

    public void connect(final IFile file, final IDocument document) throws IOException {
        start();
        if (connectedFiles.containsKey(file.getLocation())) {
            return;
        }
        // add a document buffer
        final DidOpenTextDocumentParams open = new DidOpenTextDocumentParams();
        final TextDocumentItem textDocument = new TextDocumentItem();
        textDocument.setUri(file.getLocationURI().toString());
        textDocument.setText(document.get());
        textDocument.setLanguageId(file.getFileExtension());
        open.setTextDocument(textDocument);
        server.getTextDocumentService().didOpen(open);

        final DocumentChangeListenenr listener = new DocumentChangeListenenr(
                file.getLocationURI());
        document.addDocumentListener(listener);
        connectedFiles.put(file.getLocation(), listener);
    }

    public void disconnect(final IFile file, final IDocument document) {
        document.removeDocumentListener(connectedFiles.get(file.getLocation()));
        connectedFiles.remove(file.getLocation());
        if (connectedFiles.isEmpty()) {
            stop();
        }
    }

    public LanguageServer getServer() {
        return server;
    }
}
