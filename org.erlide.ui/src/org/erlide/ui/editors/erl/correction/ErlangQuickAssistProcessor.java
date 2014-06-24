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
package org.erlide.ui.editors.erl.correction;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;

public class ErlangQuickAssistProcessor implements IQuickAssistProcessor,
        IMarkerResolutionGenerator {

    private final ErlangQuickFixCollector collector = new ErlangQuickFixCollector();

    @Override
    public boolean canAssist(final IQuickAssistInvocationContext invocationContext) {
        return collector.hasAssists(invocationContext);
    }

    @Override
    public boolean canFix(final Annotation annotation) {
        return annotation instanceof MarkerAnnotation && !annotation.isMarkedDeleted();
    }

    @Override
    public ICompletionProposal[] computeQuickAssistProposals(
            final IQuickAssistInvocationContext invocationContext) {
        final List<ICompletionProposal> result = computeProposals(invocationContext);
        return result.toArray(new ICompletionProposal[result.size()]);
    }

    @Override
    public String getErrorMessage() {
        return null;
    }

    @Override
    public IMarkerResolution[] getResolutions(final IMarker marker) {
        return collector.getResolutions(marker);
    }

    private List<ICompletionProposal> computeProposals(
            final IQuickAssistInvocationContext invocationContext) {
        final ISourceViewer sourceViewer = invocationContext.getSourceViewer();
        final Iterator<Annotation> iter = sourceViewer.getAnnotationModel()
                .getAnnotationIterator();
        final List<ICompletionProposal> result = Lists.newArrayList();
        while (iter.hasNext()) {
            final Annotation annotation = iter.next();
            if (annotation instanceof MarkerAnnotation) {
                final MarkerAnnotation markerAnnotation = (MarkerAnnotation) annotation;
                final IMarker marker = markerAnnotation.getMarker();
                try {
                    if (!marker.getType().equals(MarkerUtils.PROBLEM_MARKER)) {
                        continue;
                    }
                    final int invocationLine = sourceViewer.getDocument()
                            .getLineOfOffset(invocationContext.getOffset());
                    final int markerLine = marker.getAttribute(IMarker.LINE_NUMBER, -1) - 1;

                    if (invocationLine == markerLine) {
                        final IMarkerResolution[] qfixes = getResolutions(marker);
                        for (final IMarkerResolution qfix : qfixes) {
                            result.add(new MarkerResolutionProposal(qfix, marker));
                        }
                    }
                } catch (final BadLocationException e) {
                    ErlLogger.debug(e);
                } catch (final CoreException e) {
                    ErlLogger.debug(e);
                }
            }
        }
        result.addAll(collector.getAssists(invocationContext));
        return result;
    }

}
