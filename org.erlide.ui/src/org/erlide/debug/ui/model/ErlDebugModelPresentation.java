/**
 *
 */
package org.erlide.debug.ui.model;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.erlide.runtime.debug.ErlangDebugTarget;
import org.erlide.runtime.debug.ErlangLineBreakpoint;
import org.erlide.runtime.debug.ErlangProcess;
import org.erlide.runtime.debug.ErlangStackFrame;

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
		// TODO Auto-generated method stub
		return super.getImage(element);
	}

	/**
	 * 
	 */
	public ErlDebugModelPresentation() {
		super();
		// TODO Auto-generated constructor stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.IDebugModelPresentation#setAttribute(java.lang.String,
	 *      java.lang.Object)
	 */
	public void setAttribute(final String attribute, final Object value) {
		// TODO Auto-generated method stub

	}

	// /*
	// * (non-Javadoc)
	// *
	// * @see
	// org.eclipse.debug.ui.IDebugModelPresentation#getImage(java.lang.Object)
	// */
	// @Override
	// public Image getImage(final Object element) {
	// // TODO Auto-generated method stub
	// return null;
	// }

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.IDebugModelPresentation#getText(java.lang.Object)
	 */
	@Override
	public String getText(final Object element) {
		try {
			if (element instanceof ErlangDebugTarget) {
				return getTargetText((ErlangDebugTarget) element);
			} else if (element instanceof ErlangProcess) {
				return getErlangProcessText((ErlangProcess) element);
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

	private String getErlangLineBreakpointText(
			final ErlangLineBreakpoint breakpoint) {
		try {
			return breakpoint.getModule() + ":" + breakpoint.getLineNumber();
		} catch (final CoreException e) {
		}
		return breakpoint.getModule();
	}

	private String getErlangStackFrameText(final ErlangStackFrame stackFrame) {
		try {
			return stackFrame.getModule() + ".erl : "
					+ stackFrame.getLineNumber();
		} catch (final DebugException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * @param el
	 * @return
	 * @throws DebugException
	 */
	private String getErlangProcessText(final ErlangProcess el)
			throws DebugException {
		String r = el.isSystemProcess() ? "*" : "";
		r += el.isErlideProcess() ? "#" : "";
		return r + el.getName() + " [" + el.getStatus() + "] "
				+ el.getInitialCall() + " " + el.getCurrentFunction();
	}

	/**
	 * @param el
	 * @return
	 * @throws DebugException
	 */
	private String getTargetText(final ErlangDebugTarget el)
			throws DebugException {
		return "backend: " + el.getName();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.IDebugModelPresentation#computeDetail(org.eclipse.debug.core.model.IValue,
	 *      org.eclipse.debug.ui.IValueDetailListener)
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
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
	 *      java.lang.String)
	 */
	@Override
	public boolean isLabelProperty(final Object element, final String property) {
		// TODO Auto-generated method stub
		return false;
	}

	// /*
	// * (non-Javadoc)
	// *
	// * @see
	// org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
	// */
	// @Override
	// public void removeListener(final ILabelProviderListener listener) {
	// // TODO Auto-generated method stub
	//
	// }

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.ISourcePresentation#getEditorInput(java.lang.Object)
	 */
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.ISourcePresentation#getEditorId(org.eclipse.ui.IEditorInput,
	 *      java.lang.Object)
	 */
	public String getEditorId(final IEditorInput input, final Object element) {
		if (element instanceof IFile || element instanceof ILineBreakpoint) {
			return "org.erlide.ui.editors.erl.ErlangEditor";
		}
		return null;
	}

}
