package org.erlide.test_support.ui.trace;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.AbstractHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.util.ErlModelUtils;

public class HyperlinkDetector extends AbstractHyperlinkDetector {

    public HyperlinkDetector() {
    }

    @Override
    public IHyperlink[] detectHyperlinks(final ITextViewer textViewer,
            final IRegion region, final boolean canShowMultipleHyperlinks) {

        if (region == null || textViewer == null) {
            return null;
        }

        final IDocument document = textViewer.getDocument();
        final int offset = region.getOffset();

        if (document == null) {
            return null;
        }

        IRegion lineInfo;
        String line;
        try {
            lineInfo = document.getLineInformationOfOffset(offset);
            line = document.get(lineInfo.getOffset(), lineInfo.getLength());
        } catch (final BadLocationException ex) {
            return null;
        }
        final int offsetInLine = offset - lineInfo.getOffset();

        IRegion r = getLink(line, offsetInLine);
        if (r != null) {
            final String text = line.substring(r.getOffset(),
                    r.getOffset() + r.getLength());
            r = new Region(lineInfo.getOffset() + r.getOffset(), r.getLength());

            return new IHyperlink[] { new TraceHyperlink(r, text) };
        }
        return null;
    }

    private IRegion getLink(final String line, final int offsetInLine) {
        final int offset = 33;

        int pos = line.indexOf(" ->", offset);
        if (pos < 0) {
            if (line.indexOf('(') < 0) {
                return null;
            }
            pos = line.length();
        }
        return new Region(offset, pos - offset);
    }

    private static class TraceHyperlink implements IHyperlink {
        private final IRegion region;
        private final String text;

        public TraceHyperlink(final IRegion region, final String text) {
            this.text = text;
            this.region = region;
        }

        @Override
        public String getTypeLabel() {
            return null;
        }

        @Override
        public String getHyperlinkText() {
            return text;
        }

        @Override
        public void open() {
            final String[] mf = text.split(":");
            final String module = mf[0];
            String function = mf[1];
            int arity;

            final int slash = function.indexOf('/');
            if (function.matches("'-[^/]+/[0-9]+-fun-.*")) {
                arity = Integer.parseInt(function.substring(slash + 1,
                        function.indexOf('-', slash)));
                function = function.substring(2, slash);
            } else {
                if (slash < 0) {
                    final int pos = function.indexOf('(');
                    arity = countArgs(function.substring(pos + 1,
                            function.length() - 1));
                    function = function.substring(0, pos);
                } else {
                    final String args = function.substring(slash + 1);
                    arity = Integer.parseInt(args);
                    function = function.substring(0, slash);
                }
            }

            try {
                ErlModelUtils.openMFA(module, function, arity);
            } catch (final CoreException e) {
                ErlLogger.warn("Could not open %s:%s/%d", module, function,
                        arity);
            }
        }

        private int countArgs(final String args) {
            if (args.length() == 0) {
                return 0;
            }
            return countArgs(args, 0, 1, 0);
        }

        private int countArgs(final String args, int i, final int n, final int p) {
            if (args.length() == i) {
                return n;
            }
            final char c = args.charAt(i++);
            if (c == '[') {
                return countArgs(args, i, n, p + 1);
            } else if (c == '{') {
                return countArgs(args, i, n, p + 1);
            } else if (c == '}') {
                return countArgs(args, i, n, p - 1);
            } else if (c == ']') {
                return countArgs(args, i, n, p - 1);
            } else if (c == ',') {
                if (p == 0) {
                    return countArgs(args, i, n + 1, p);
                }
                return countArgs(args, i, n, p);
            } else {
                return countArgs(args, i, n, p);
            }
        }

        @Override
        public IRegion getHyperlinkRegion() {
            return region;
        }
    }
}
