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
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.debug.ErlangDebugTarget;
import org.erlide.runtime.debug.ErlangLineBreakpoint;
import org.erlide.runtime.debug.ErlangProcess;
import org.erlide.runtime.debug.ErlangStackFrame;
import org.erlide.runtime.debug.ErlangUninterpretedStackFrame;
import org.erlide.ui.ErlideUIDebugImages;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;

/**
 * @author jakob
 * 
 */
public class ErlDebugModelPresentation extends LabelProvider implements
        IDebugModelPresentation {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
     */
    @Override
    public Image getImage(final Object element) {
        if (element instanceof ErlangUninterpretedStackFrame) {
            return ErlideUIDebugImages
                    .get(ErlideUIDebugImages.IMG_OBJ_UNINTERPRETED_STACK_FRAME);
        }
        return super.getImage(element);
    }

    public ErlDebugModelPresentation() {
        super();
        // TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.debug.ui.IDebugModelPresentation#setAttribute(java.lang.String
     * , java.lang.Object)
     */
    public void setAttribute(final String attribute, final Object value) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.debug.ui.IDebugModelPresentation#getText(java.lang.Object)
     */
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

    /**
     * @param el
     * @return
     * @throws DebugException
     */
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

    /**
     * @param el
     * @return
     * @throws DebugException
     */
    private String getTargetText(final ErlangDebugTarget el)
            throws DebugException {
        return el.getName() + " (backend)";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.debug.ui.IDebugModelPresentation#computeDetail(org.eclipse
     * .debug.core.model.IValue, org.eclipse.debug.ui.IValueDetailListener)
     */
    public void computeDetail(final IValue value,
            final IValueDetailListener listener) {
        String detail = "";
        try {
            detail = value.getValueString();
        } catch (final DebugException e) {
        }
        listener.detailComputed(value, detail);
    }

    // /*
    // * (non-Javadoc)
    // *
    // * @see
    // org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
    // */
    // @Override
    // public void addListener(final ILabelProviderListener listener) {
    // // TODO Auto-generated method stub
    //
    // }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
     */
    @Override
    public void dispose() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang
     * .Object, java.lang.String)
     */
    @Override
    public boolean isLabelProperty(final Object element, final String property) {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.debug.ui.ISourcePresentation#getEditorInput(java.lang.Object)
     */
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
                final IErlModule module = ModelUtils.openExternal(null, lfs
                        .getFullPath().toString());
                return EditorUtility.getEditorInput(module);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.debug.ui.ISourcePresentation#getEditorId(org.eclipse.ui.
     * IEditorInput, java.lang.Object)
     */
    public String getEditorId(final IEditorInput input, final Object element) {
        if (element instanceof IFile || element instanceof ILineBreakpoint
                || element instanceof LocalFileStorage) {
            return ErlangEditor.ERLANG_EDITOR_ID;
        }
        return null;
    }

}
