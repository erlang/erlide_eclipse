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

import org.eclipse.compare.ITypedElement;
import org.eclipse.compare.structuremergeviewer.DocumentRangeNode;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.swt.graphics.Image;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.ui.editors.erl.outline.ErlangElementImageProvider;
import org.erlide.ui.internal.ErlideUIPlugin;

class ErlNode extends DocumentRangeNode implements ITypedElement {

    private final ErlElementKind kind;
    private final String name;

    private ErlNode(final ErlNode parent, final ErlElementKind kind, final String name,
            final String id, final IDocument doc, final int start, final int length) {
        super(kind.hashCode(), id, doc, start, length);
        this.kind = kind;
        this.name = name;
        parent.addChild(this);
    }

    public static ErlNode createErlNode(final ErlNode parent, final IErlElement element,
            final IDocument doc) {
        int start = 0, length = 0;
        String name = element.toString();
        if (element instanceof IErlModule) {
            final IErlModule m = (IErlModule) element;
            length = doc.getLength();
            name = m.getModuleName();
        } else if (element instanceof ISourceReference) {
            final ISourceReference sourceReference = (ISourceReference) element;
            final ISourceRange sr = sourceReference.getSourceRange();
            start = sr.getOffset();
            length = sr.getLength();
        }
        return new ErlNode(parent, element.getKind(), name,
                ErlangCompareUtilities.getErlElementID(element), doc, start, length);
    }

    public ErlNode(final IDocument doc) {
        super(ErlElementKind.MODEL.hashCode(), "<root>", doc, 0, doc.getLength());
        kind = ErlElementKind.MODEL;
        name = "<root>";
    }

    /**
     * Extends the range to include ranges of children, this is needed since the
     * range of a function in the erlang model only covers the first clause
     * (which is good in the outline and the navigator, but not optimal here).
     *
     * @see org.eclipse.compare.structuremergeviewer.DocumentRangeNode#addChild(org.eclipse.compare.structuremergeviewer.DocumentRangeNode)
     */
    @Override
    public void addChild(final DocumentRangeNode node) {
        final Position p = rangeUnion(getRange(), node.getRange());
        final Position r = getRange();
        r.setOffset(p.getOffset());
        r.setLength(p.getLength());
        super.addChild(node);
    }

    private Position rangeUnion(final Position a, final Position b) {
        final int offsetA = a.getOffset(), offsetB = b.getOffset();
        final int endA = offsetA + a.getLength(), endB = offsetB + b.getLength();
        final int end = Math.max(endA, endB);
        final int offset = Math.min(offsetA, offsetB);
        return new Position(offset, end - offset);
    }

    /**
     * @see ITypedElement#getNodeType
     */
    public ErlElementKind getNodeType() {
        return kind;
    }

    /**
     * @see ITypedElement#getName
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * @see ITypedElement#getType
     */
    @Override
    public String getType() {
        return "erl2";
    }

    /**
     * @see ITypedInput#getImage
     */
    @Override
    public Image getImage() {
        // final ImageDescriptor descriptor = ErlideUIPlugin.getDefault()
        // .getImageDescriptor("erl");
        final ImageDescriptor descriptor = ErlangElementImageProvider
                .getImageDescriptionFromKind(kind);
        return ErlideUIPlugin.getImageDescriptorRegistry().get(descriptor);
    }
}
