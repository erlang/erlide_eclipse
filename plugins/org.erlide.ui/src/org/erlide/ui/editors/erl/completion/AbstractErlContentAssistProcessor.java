package org.erlide.ui.editors.erl.completion;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContentAssistEvent;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.ICompletionListener;
import org.eclipse.jface.text.contentassist.ICompletionListenerExtension;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.backend.BackendCore;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlElement;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.codeassist.CompletionData;
import org.erlide.engine.services.codeassist.CompletionService;
import org.erlide.engine.services.codeassist.FunctionCompletionData;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.ui.internal.information.HoverUtil;
import org.erlide.ui.templates.ErlTemplateCompletionProcessor;
import org.erlide.util.ErlLogger;
import org.erlide.util.event_tracer.ErlideEventTracer;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

public abstract class AbstractErlContentAssistProcessor
        implements IContentAssistProcessor {

    public boolean restarted;

    private final class CompletionListener
            implements ICompletionListener, ICompletionListenerExtension {

        @Override
        public void assistSessionStarted(final ContentAssistEvent event) {
            if (event.processor != AbstractErlContentAssistProcessor.this) {
                return;
            }
            restarted = false;
        }

        @Override
        public void assistSessionEnded(final ContentAssistEvent event) {
            if (event.processor != AbstractErlContentAssistProcessor.this) {
                return;
            }
            restarted = false;
        }

        @Override
        public void selectionChanged(final ICompletionProposal proposal,
                final boolean smartToggle) {
        }

        @Override
        public void assistSessionRestarted(final ContentAssistEvent event) {
            if (event.processor != AbstractErlContentAssistProcessor.this) {
                return;
            }
            restarted = true;
        }
    }

    protected final ISourceViewer sourceViewer;
    protected final IErlModule module;
    protected final IErlProject project;

    protected final ContentAssistant contentAssistant;
    private IDocument oldDoc;
    private String oldBefore;
    private int oldSuggestions = -1;

    public AbstractErlContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final IErlProject project,
            final ContentAssistant contentAssistant) {
        this.sourceViewer = sourceViewer;
        this.module = module;
        this.project = project;
        this.contentAssistant = contentAssistant;
        if (contentAssistant != null) {
            contentAssistant.addCompletionListener(new CompletionListener());
        }
    }

    @Override
    public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer,
            final int offset) {
        final String id = Integer.toHexString(viewer.hashCode()) + "@" + offset;
        try {
            ErlideEventTracer.getInstance().traceOperationStart("completion", id);
            try {
                final IDocument doc = viewer.getDocument();
                final String before = getBefore(viewer, doc, offset);

                String elementBefore;
                final IErlElement el = getElementAt(offset);
                if (el instanceof ISourceReference) {
                    final ISourceRange r = ((ISourceReference) el).getSourceRange();
                    final int o = r.getOffset();
                    elementBefore = doc.get(o, offset - o);
                } else {
                    elementBefore = null;
                }
                // ErlLogger.debug("computeCompletionProposals before = %s %d %s",
                // before, oldSuggestions, oldDoc);

                if (restarted && offset > 0) {
                    final char last = doc.get(offset - 1, 1).charAt(0);
                    if (last == ',' || last == '.' || last == ';' || last == ')'
                            || last == '(') {
                        return null;
                    }
                }

                if (Objects.equal(oldDoc, doc) && oldBefore != null
                        && before.startsWith(oldBefore) && oldSuggestions == 0) {
                    return getNoCompletion(offset);
                }
                oldDoc = doc;
                oldBefore = before;

                final CompletionService completionService = ErlangEngine.getInstance()
                        .getCompletionService();
                List<ICompletionProposal> result = Lists.newArrayList();
                if (project != null) {
                    final IOtpRpc backend = BackendCore.getBuildBackend(project);
                    final List<CompletionData> resultData = completionService
                            .computeCompletions(backend, project, module, elementBefore,
                                    offset, before, isInString());
                    result = Lists.transform(resultData, this::toProposal);
                }
                final ErlTemplateCompletionProcessor t = new ErlTemplateCompletionProcessor(
                        doc, offset - before.length(), before.length());
                result.addAll(
                        Arrays.asList(t.computeCompletionProposals(viewer, offset)));
                oldSuggestions = result.size();
                if (result.isEmpty()) {
                    ErlLogger.debug("no results");
                    return getNoCompletion(offset);
                }
                // ErlLogger.debug("%d results", result.size());
                return result.toArray(new ICompletionProposal[result.size()]);
            } catch (final Exception e) {
                ErlLogger.warn(e);
                return null;
            }
        } finally {
            ErlideEventTracer.getInstance().traceOperationEnd("completion", id);
        }
    }

    protected boolean isInString() {
        return false;
    }

    private IErlElement getElementAt(final int offset) {
        if (module == null) {
            return null;
        }
        try {
            return module.getElementAt(offset);
        } catch (final ErlModelException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    protected ICompletionProposal toProposal(final CompletionData data) {
        if (data instanceof FunctionCompletionData) {
            final FunctionCompletionData fdata = (FunctionCompletionData) data;
            final String info = fdata.getAdditionalProposalInfo();
            final StringBuffer buffer = new StringBuffer(info == null ? "" : info);
            return new ErlCompletionProposal(fdata.getOffsetsAndLengths(),
                    fdata.getDisplayString(), fdata.getReplacementString(),
                    fdata.getReplacementOffset(), fdata.getReplacementLength(),
                    fdata.getCursorPosition(), null, null, HoverUtil.getHTML(buffer),
                    sourceViewer);

        }
        return new CompletionProposal(data.getReplacementString(),
                data.getReplacementOffset(), data.getReplacementLength(),
                data.getCursorPosition());
    }

    private ICompletionProposal[] getNoCompletion(final int offset) {
        return new ICompletionProposal[] { new DummyCompletionProposal(offset) };
    }

    String getBefore(final ITextViewer viewer, final IDocument doc, final int offset) {
        try {
            if (module != null) {
                try {
                    final IErlElement element = module.getElementAt(offset);
                    if (element instanceof ISourceReference) {
                        final ISourceReference sr = (ISourceReference) element;
                        final int start = sr.getSourceRange().getOffset();
                        if (start <= offset) {
                            return doc.get(start, offset - start);
                        }
                    }
                } catch (final ErlModelException e) {
                }
            }
            for (int n = offset - 1; n >= 0; --n) {
                final char c = doc.getChar(n);
                final int type = Character.getType(c);
                if (type == Character.LINE_SEPARATOR
                        || type == Character.PARAGRAPH_SEPARATOR
                        || type == Character.CONTROL) {
                    return doc.get(n + 1, offset - n - 1);
                }
            }
            return doc.get(0, offset);
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        }
        return "";
    }

    @Override
    public IContextInformation[] computeContextInformation(final ITextViewer viewer,
            final int offset) {
        return null;
    }

    @Override
    public char[] getContextInformationAutoActivationCharacters() {
        return null;
    }

    @Override
    public String getErrorMessage() {
        return null;
    }

    @Override
    public IContextInformationValidator getContextInformationValidator() {
        return null;
    }

}
