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
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;

public class ErlangQuickAssistProcessor implements IQuickAssistProcessor {
    ISourceViewer sourceViewer;

    public ErlangQuickAssistProcessor(final ISourceViewer sourceViewer) {
        this.sourceViewer = sourceViewer;
    }

    @Override
    public boolean canAssist(final IQuickAssistInvocationContext invocationContext) {
        return false;
    }

    @Override
    public boolean canFix(final Annotation annotation) {
        return true; // annotation instanceof ErlangAnnotation;
    }

    @Override
    public ICompletionProposal[] computeQuickAssistProposals(
            final IQuickAssistInvocationContext invocationContext) {
        final List<ICompletionProposal> result = Lists.newArrayList();

        final QuickFixResolutionGenerator gen = new QuickFixResolutionGenerator();
        final Iterator<Annotation> iter = sourceViewer.getAnnotationModel()
                .getAnnotationIterator();
        while (iter.hasNext()) {
            final Annotation annotation = iter.next();
            if (annotation instanceof MarkerAnnotation) {
                final MarkerAnnotation markerAnnotation = (MarkerAnnotation) annotation;
                final IMarker marker = markerAnnotation.getMarker();
                try {
                    if (!marker.getType().equals(MarkerUtils.PROBLEM_MARKER)) {
                        continue;
                    }
                    final int line = marker.getAttribute(IMarker.LINE_NUMBER, -1) - 1;
                    final int lineOffset = sourceViewer.getDocument().getLineOffset(line);
                    final int lineEnd = lineOffset
                            + sourceViewer.getDocument().getLineLength(line);
                    final int invocationOffset = invocationContext.getOffset();

                    if (lineOffset <= invocationOffset && invocationOffset < lineEnd) {
                        final IMarkerResolution[] qfixes = gen.getResolutions(marker);
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

        return result.toArray(new ICompletionProposal[result.size()]);
    }

    @Override
    public String getErrorMessage() {
        return null;
    }

}
