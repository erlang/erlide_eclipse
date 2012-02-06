/**
 *
 */
package org.erlide.debug.ui.model;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.sourcelookup.containers.LocalFileStorage;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.ErlangLineBreakpoint;
import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.erlide.launch.debug.model.ErlangProcess;
import org.erlide.launch.debug.model.ErlangStackFrame;
import org.erlide.launch.debug.model.ErlangUninterpretedStackFrame;
import org.erlide.ui.ErlideUIDebugImages;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;

/**
 * @author jakob
 * 
 */
public class ErlDebugModelPresentation extends LabelProvider implements
        IDebugModelPresentation {

    @Override
    public Image getImage(final Object element) {
        if (element instanceof ErlangUninterpretedStackFrame) {
            return ErlideUIDebugImages
                    .get(ErlideUIDebugImages.IMG_OBJ_UNINTERPRETED_STACK_FRAME);
        }
        return super.getImage(element);
    }

    @Override
    public void setAttribute(final String attribute, final Object value) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getText(final Object element) {
        try {
            if (element instanceof ErlangDebugTarget) {
                return getTargetText((ErlangDebugTarget) element);
            } else if (element instanceof ErlangProcess) {
                return getErlangProcessText((ErlangProcess) element);
            } else if (element instanceof ErlangUninterpretedStackFrame) {
                return getErlangUninterpretedStackFrameText((ErlangUninterpretedStackFrame) element);
            } else if (element instanceof ErlangStackFrame) {
                return getErlangStackFrameText((ErlangStackFrame) element);
            } else if (element instanceof ErlangLineBreakpoint) {
                return getErlangLineBreakpointText((ErlangLineBreakpoint) element);
            }
            return null;
        } catch (final DebugException e) {
            return "?";
        }
    }

    private String getErlangUninterpretedStackFrameText(
            final ErlangUninterpretedStackFrame stackFrame) {
        return stackFrame.getModule() + ":"
                + stackFrame.getFunction().getNameWithArity();
    }

    private String getErlangLineBreakpointText(
            final ErlangLineBreakpoint breakpoint) {
        try {
            return getErlangPositionText(breakpoint.getModule(),
                    breakpoint.getLineNumber(), breakpoint.getClauseHead());
        } catch (final CoreException e) {
        }
        return breakpoint.getModule();
    }

    private String getErlangStackFrameText(final ErlangStackFrame stackFrame) {
        try {
            return getErlangPositionText(stackFrame.getModule(),
                    stackFrame.getLineNumber(), stackFrame.getClauseHead());
        } catch (final DebugException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    private static String getErlangPositionText(final String module,
            final int lineNumber, final String clauseHead)
            throws DebugException {
        final StringBuilder sb = new StringBuilder();
        sb.append(module);
        if (lineNumber != -1) {
            sb.append(':');
            sb.append(lineNumber);
        }
        if (clauseHead != null && clauseHead.length() > 0) {
            sb.append(" - ").append(clauseHead);
        }
        return sb.toString();
    }

    private String getErlangProcessText(final ErlangProcess el)
            throws DebugException {
        final StringBuilder sb = new StringBuilder();
        if (el.isSystemProcess()) {
            sb.append('*');
        }
        if (el.isErlideProcess()) {
            sb.append('#');
        }
        sb.append(el.getName());
        sb.append(" [").append(el.getStatus());
        final boolean terminated = el.getStatus().equals(
                ErlangProcess.STATUS_TERMINATED);
        if (terminated) {
            sb.append(", ").append(el.getExitStatus());
        }
        sb.append("] ").append(el.getInitialCall());
        if (!terminated) {
            sb.append(' ').append(el.getCurrentFunction());
        }
        return sb.toString();
    }

    private String getTargetText(final ErlangDebugTarget el)
            throws DebugException {
        return el.getName() + " (backend)";
    }

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
    public void dispose() {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean isLabelProperty(final Object element, final String property) {
        return true;
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
        if (element instanceof LocalFileStorage) {
            final LocalFileStorage lfs = (LocalFileStorage) element;
            try {
                final IErlModule module = ModelUtils.findModule(null, null, lfs
                        .getFullPath().toString(),
                        IErlElementLocator.Scope.ALL_PROJECTS);
                return EditorUtility.getEditorInput(module);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public String getEditorId(final IEditorInput input, final Object element) {
        if (element instanceof IFile || element instanceof ILineBreakpoint
                || element instanceof LocalFileStorage) {
            return ErlangEditor.ERLANG_EDITOR_ID;
        }
        return null;
    }

}
