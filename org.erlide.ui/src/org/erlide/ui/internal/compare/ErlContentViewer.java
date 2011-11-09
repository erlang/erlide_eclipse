package org.erlide.ui.internal.compare;

import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.ErlangSourceViewerConfiguration;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlContentViewer extends Viewer {

    private final SourceViewer fSourceViewer;
    private Object fInput;

    ErlContentViewer(final Composite parent) {
        fSourceViewer = new SourceViewer(parent, null, SWT.LEFT_TO_RIGHT
                | SWT.H_SCROLL | SWT.V_SCROLL);
        final IPreferenceStore store = ErlideUIPlugin.getDefault()
                .getPreferenceStore();
        fSourceViewer.configure(new ErlangSourceViewerConfiguration(store,
                new ColorManager()));
        fSourceViewer.setEditable(false);

        final String symbolicFontName = ErlMergeViewer.class.getName();
        final Font font = JFaceResources.getFont(symbolicFontName);
        if (font != null) {
            fSourceViewer.getTextWidget().setFont(font);
        }

    }

    @Override
    public Control getControl() {
        return fSourceViewer.getControl();
    }

    @Override
    public void setInput(final Object input) {
        if (input instanceof IStreamContentAccessor) {
            final Document document = new Document(getString(input));
            // ErlangCompareUtilities.setupDocument(document);
            fSourceViewer.setDocument(document);
        }
        fInput = input;
    }

    @Override
    public Object getInput() {
        return fInput;
    }

    @Override
    public ISelection getSelection() {
        return fSourceViewer.getSelection();
    }

    @Override
    public void setSelection(final ISelection s, final boolean reveal) {
        fSourceViewer.setSelection(s, reveal);
    }

    @Override
    public void refresh() {
        fSourceViewer.refresh();
    }

    /**
     * A helper method to retrieve the contents of the given object if it
     * implements the IStreamContentAccessor interface.
     */
    private static String getString(final Object input) {

        if (input instanceof IStreamContentAccessor) {
            final IStreamContentAccessor sca = (IStreamContentAccessor) input;
            try {
                return ErlangCompareUtilities.readString(sca);
            } catch (final CoreException ex) {
                ErlLogger.error(ex);
            }
        }
        return ""; //$NON-NLS-1$
    }
}
