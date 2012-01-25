package org.erlide.debug.ui.tracing;

import org.eclipse.core.resources.IFile;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.erlide.launch.debug.model.ErlangStackFrame;
import org.erlide.ui.editors.erl.ErlangEditor;

public class DebugTraceDebugModelPresentation extends LabelProvider implements
        IDebugModelPresentation {

    public static final String ID = "org.erlide.debug.trace.model";

    @Override
    public void computeDetail(final IValue value,
            final IValueDetailListener listener) {
        String detail = "";
        try {
            detail = value.getValueString();
        } catch (final DebugException e) {
        }
        listener.detailComputed(value, detail);
    }

    @Override
    public void setAttribute(final String attribute, final Object value) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getEditorId(final IEditorInput input, final Object element) {
        if (element instanceof IFile || element instanceof ILineBreakpoint) {
            return ErlangEditor.ERLANG_EDITOR_ID;
        }
        return null;
    }

    @Override
    public IEditorInput getEditorInput(final Object element) {

        if (element instanceof IFile) {
            return new FileEditorInput((IFile) element);
        }
        if (element instanceof ILineBreakpoint) {
            return new FileEditorInput((IFile) ((ILineBreakpoint) element)
                    .getMarker().getResource());
        }
        return null;
    }

    @Override
    public String getText(final Object element) {
        if (element instanceof DebugTraceTarget) {
            return getTargetText((DebugTraceTarget) element);
        } else if (element instanceof DebugTraceProcess) {
            return getDebugTraceProcessText((DebugTraceProcess) element);
        } else if (element instanceof ErlangStackFrame) {
            return getErlangStackFrameText((ErlangStackFrame) element);
        }
        return null;
    }

    private String getErlangStackFrameText(final ErlangStackFrame element) {
        return element.toString();
    }

    private String getDebugTraceProcessText(final DebugTraceProcess element) {
        return element.toString();
    }

    private String getTargetText(final DebugTraceTarget element) {
        return element.toString();
    }

}
