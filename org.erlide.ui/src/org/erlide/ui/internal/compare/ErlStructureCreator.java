/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     QNX Software System
 *******************************************************************************/
package org.erlide.ui.internal.compare;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.compare.CompareUI;
import org.eclipse.compare.IEditableContent;
import org.eclipse.compare.IEditableContentExtension;
import org.eclipse.compare.ISharedDocumentAdapter;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.ITypedElement;
import org.eclipse.compare.ResourceNode;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.compare.structuremergeviewer.DocumentRangeNode;
import org.eclipse.compare.structuremergeviewer.IDiffContainer;
import org.eclipse.compare.structuremergeviewer.IStructureComparator;
import org.eclipse.compare.structuremergeviewer.IStructureCreator;
import org.eclipse.compare.structuremergeviewer.StructureCreator;
import org.eclipse.compare.structuremergeviewer.StructureRootNode;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.services.IDisposable;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IOpenable;
import org.erlide.core.model.root.IParent;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.ErlangDocumentSetupParticipant;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlStructureCreator extends StructureCreator {

    private static final String NAME = "Erlang Structure Compare";

    // private final IErlProject fProject;

    private IDocumentPartitioner documentPartitioner = null;

    public ErlStructureCreator() {
    }

    /**
     * @see IStructureCreator#getTitle
     */
    @Override
    public String getName() {
        return ErlideUIPlugin.getResourceString(NAME);
    }

    /**
     * A root node for the structure. It is similar to {@link StructureRootNode}
     * but needed to be a subclass of {@link ErlNode} because of the code used
     * to build the structure.
     */
    private final class RootErlNode extends ErlNode implements IDisposable {
        private Object fInput;

        private RootErlNode(final IDocument document, final Object input) {
            super(document);
            fInput = input;
        }

        @Override
        protected void nodeChanged(final DocumentRangeNode node) {
            save(this, fInput);
        }

        @Override
        public boolean isReadOnly() {
            if (fInput instanceof IEditableContentExtension) {
                final IEditableContentExtension ext = (IEditableContentExtension) fInput;
                return ext.isReadOnly();
            }
            return super.isReadOnly();
        }

        @Override
        public IStatus validateEdit(final Shell shell) {
            if (fInput instanceof IEditableContentExtension) {
                final IEditableContentExtension ext = (IEditableContentExtension) fInput;
                return ext.validateEdit(shell);
            }
            return super.validateEdit(shell);
        }

        @Override
        public void dispose() {
            fInput = null;
        }
    }

    /**
     * @see IStructureCreator#getStructure
     */
    @Override
    public IStructureComparator getStructure(final Object input) {
        String contents = null;
        char[] buffer = null;
        IDocument doc = CompareUI.getDocument(input);
        if (doc == null) {
            if (input instanceof IStreamContentAccessor) {
                final IStreamContentAccessor sca = (IStreamContentAccessor) input;
                try {
                    contents = ErlangCompareUtilities.readString(sca);
                } catch (final CoreException ex) {
                    // return null indicates the error.
                    return null;
                }
            }

            if (contents != null) {
                final int n = contents.length();
                buffer = new char[n];
                contents.getChars(0, n, buffer, 0);

                doc = new Document(contents);
                setupDocument(doc);
            }
        }

        try {
            return createStructureComparator(input, doc, null, null);
        } catch (final CoreException e) {
            ErlLogger.error(e); // TODO report error
        }
        return null;
    }

    private ErlNode recursiveMakeErlNodes(final IErlElement element,
            final ErlNode parent, final IDocument doc) throws ErlModelException {
        final ErlNode n = ErlNode.createErlNode(parent, element, doc);
        if (element instanceof IOpenable) {
            final IOpenable o = (IOpenable) element;
            o.open(null);
        }
        if (element instanceof IParent) {
            final IParent p = (IParent) element;
            final Collection<IErlElement> children = p.getChildren();
            for (final IErlElement child : children) {
                recursiveMakeErlNodes(child, n, doc);
            }
        }
        return n;
    }

    /**
     * @see IStructureCreator#canSave
     */
    public boolean canSave() {
        return true;
    }

    /**
     * @see IStructureCreator#canRewriteTree
     */
    public boolean canRewriteTree() {
        return false;
    }

    /**
     * @see IStructureCreator#rewriteTree
     */
    public void rewriteTree(final Differencer differencer,
            final IDiffContainer root) {
    }

    /**
     * @see IStructureCreator#save
     */
    @Override
    public void save(final IStructureComparator structure, final Object input) {
        if (input instanceof IEditableContent && structure instanceof ErlNode) {
            final IDocument doc = ((ErlNode) structure).getDocument();
            final IEditableContent bca = (IEditableContent) input;
            final String c = doc.get();
            bca.setContent(c.getBytes());
        }
    }

    /**
     * @see IStructureCreator#getContents
     */
    @Override
    public String getContents(final Object node, final boolean ignoreWhitespace) {
        if (node instanceof IStreamContentAccessor) {
            final IStreamContentAccessor sca = (IStreamContentAccessor) node;
            try {
                return readString(sca.getContents());
            } catch (final CoreException ex) {
            }
        }
        return null;
    }

    /**
     * Returns null if an error occurred.
     */
    private static String readString(final InputStream is) {
        if (is == null) {
            return null;
        }
        BufferedReader reader = null;
        try {
            final StringBuilder buffer = new StringBuilder();
            final char[] part = new char[2048];
            int read = 0;
            reader = new BufferedReader(new InputStreamReader(is));

            while ((read = reader.read(part)) != -1) {
                buffer.append(part, 0, read);
            }

            return buffer.toString();

        } catch (final IOException ex) {
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (final IOException ex) {
                }
            }
        }
        return null;
    }

    @Override
    protected IStructureComparator createStructureComparator(
            final Object element, IDocument document,
            final ISharedDocumentAdapter sharedDocumentAdapter,
            final IProgressMonitor monitor) throws CoreException {
        IErlModule module = null;
        String s = "";
        final IErlModel model = ErlModelManager.getErlangModel();
        if (element instanceof ResourceNode) {
            final ResourceNode rn = (ResourceNode) element;
            final IResource r = rn.getResource();
            if (r instanceof IFile) {
                final IFile f = (IFile) r;
                final IErlElement e = model.findElement(r);
                if (e instanceof IErlModule) {
                    module = (IErlModule) e;
                }
                if (document == null) {
                    try {
                        s = readString(f.getContents());
                        document = new Document(s);
                    } catch (final CoreException e1) {
                    }
                }
            }
        } else if (document == null
                && element instanceof IStreamContentAccessor) {
            try {
                final InputStream contents = ((IStreamContentAccessor) element)
                        .getContents();
                s = readString(contents);
                document = new Document(s);
            } catch (final CoreException ex) {
            }
        } else if (document != null) {
            s = document.get();
        }
        if (module == null) {
            String name = "comptemp";
            if (element instanceof ITypedElement) {
                final ITypedElement typedElement = (ITypedElement) element;
                name = typedElement.getName();
            }
            module = model.getModuleFromText(model, name, s, s);
        }
        ErlNode root = null;
        if (element != null && document != null) {
            try {
                module.open(null);
                root = new RootErlNode(document, element);
                recursiveMakeErlNodes(module, root, document);
            } catch (final ErlModelException e) {
                ErlLogger.warn(e);
            }
        }
        return root;
    }

    @Override
    protected String[] getPath(final Object element, final Object input) {
        if (element instanceof IErlElement) {
            IErlElement e = (IErlElement) element;
            // build a path starting at the given element and walk
            // up the parent chain until we reach a module
            final List<String> args = new ArrayList<String>();
            while (e != null) {
                // each path component has a name that uses the same
                // conventions as a ErlNode name
                final String name = ErlangCompareUtilities.getErlElementID(e);
                if (name == null) {
                    return null;
                }
                args.add(name);
                if (e instanceof IErlModule) {
                    break;
                }
                e = (IErlElement) e.getParent();
            }
            Collections.reverse(args);
            return args.toArray(new String[args.size()]);
        }
        return null;
    }

    @Override
    protected String getDocumentPartitioning() {
        return IErlangPartitions.ERLANG_PARTITIONING;
    }

    @Override
    protected IDocumentPartitioner getDocumentPartitioner() {
        if (documentPartitioner == null) {
            documentPartitioner = ErlangDocumentSetupParticipant
                    .createDocumentPartitioner();
        }
        return documentPartitioner;
    }
}
