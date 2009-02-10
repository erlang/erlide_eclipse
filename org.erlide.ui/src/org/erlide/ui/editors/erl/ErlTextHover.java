/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.net.URL;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IVariable;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.internal.text.html.HTMLPrinter;
import org.eclipse.jface.internal.text.html.HTMLTextPresenter;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.runtime.backend.Backend;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.ErlModelUtils;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDoc;

public class ErlTextHover implements ITextHover,
		IInformationProviderExtension2, ITextHoverExtension {

	// private ITextEditor fEditor;
	private List<IErlImport> fImports;
	private final IErlModule fModule;
	private final String fExternalIncludes;
	private static URL fgStyleSheet;

	public ErlTextHover(final IErlModule module, final String externalIncludes) {
		fImports = null;
		fModule = module;
		fExternalIncludes = externalIncludes;
		initStyleSheet();
	}

	public IRegion getHoverRegion(final ITextViewer textViewer, final int offset) {
		final ErlToken token = fModule.getScanner().getTokenAt(offset);
		if (token == null) {
			return null;
		}
		// ErlLogger.debug("getHoverRegion " + token.toString());
		return new Region(token.getOffset(), token.getLength());
	}

	@SuppressWarnings("restriction")
	public String getHoverInfo(final ITextViewer textViewer,
			final IRegion hoverRegion) {
		StringBuffer result = new StringBuffer();
		if (fImports == null) {
			fImports = ErlModelUtils.getImportsAsList(fModule);
		}
		final int offset = hoverRegion.getOffset();
		OtpErlangObject r1 = null;
		final String debuggerVar = makeDebuggerVariableHover(textViewer,
				offset, hoverRegion.getLength());
		if (debuggerVar.length() > 0) {
			result.append(debuggerVar);
		}
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		r1 = ErlideDoc.getDocFromScan(b, offset, stateDir, ErlScanner
				.createScannerModuleName(fModule), fImports);
		// ErlLogger.debug("getHoverInfo getDocFromScan " + r1);
		if (r1 instanceof OtpErlangString) {
			final OtpErlangString s1 = (OtpErlangString) r1;
			result.append(s1.stringValue());
		} else if (r1 instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) r1;
			final OtpErlangObject o0 = t.elementAt(0);
			final OtpErlangObject o1 = t.elementAt(1);
			if (o0 instanceof OtpErlangAtom && o1 instanceof OtpErlangAtom) {
				final OtpErlangAtom a0 = (OtpErlangAtom) o0;
				final OtpErlangAtom a1 = (OtpErlangAtom) o1;
				String definedName = a1.atomValue();
				if (definedName.charAt(0) == '?') {
					definedName = definedName.substring(1);
				}
				final IErlElement.Kind kindToFind = a0.atomValue().equals(
						"record") ? IErlElement.Kind.RECORD_DEF
						: IErlElement.Kind.MACRO_DEF;
				final IProject proj = (IProject) fModule.getProject()
						.getResource();
				final IErlPreprocessorDef pd = ErlModelUtils
						.findPreprocessorDef(b, proj, fModule, definedName,
								kindToFind, fExternalIncludes,
								ErlContentAssistProcessor.getPathVars());
				if (pd != null) {
					result.append(pd.getExtra());
				}
			}
		}
		if (result.length() > 0) {
			HTMLPrinter.insertPageProlog(result, 0, fgStyleSheet);
			HTMLPrinter.addPageEpilog(result);
		}
		return result.toString();
	}

	private void initStyleSheet() {
		Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
		fgStyleSheet = bundle.getEntry("/edoc.css"); //$NON-NLS-1$
		if (fgStyleSheet != null) {

			try {
				fgStyleSheet = FileLocator.toFileURL(fgStyleSheet);
			} catch (Exception e) {
			}
		}
	}

	/**
	 * @param textViewer
	 * @param offset
	 * @param length
	 */
	private String makeDebuggerVariableHover(final ITextViewer textViewer,
			final int offset, final int length) {
		final IAdaptable adaptable = DebugUITools.getDebugContext();
		if (adaptable != null) {
			final IStackFrame frame = (IStackFrame) adaptable
					.getAdapter(IStackFrame.class);
			try {
				if (frame != null && frame.hasVariables()) {
					String varName = "";
					try {
						varName = textViewer.getDocument().get(offset, length);
					} catch (final BadLocationException e) {
					}
					if (varName.length() > 0) {
						final String firstLetter = varName.substring(0, 1);
						if (firstLetter.toUpperCase().equals(firstLetter)) {
							final IVariable[] vars = frame.getVariables();
							for (final IVariable variable : vars) {
								if (variable.getName().equals(varName)) {
									final String value = variable.getValue()
											.getValueString();
									return makeVariablePresentation(varName,
											value);
								}
							}
						}
					}
				}
			} catch (final DebugException e) {
			}
		}
		return "";
	}

	private String makeVariablePresentation(final String varName,
			final String value) {
		return varName + " = " + value;
	}

	public IInformationControlCreator getInformationPresenterControlCreator() {
		return new IInformationControlCreator() {

			@SuppressWarnings("restriction")
			public IInformationControl createInformationControl(
					final Shell parent) {
				final int shellStyle = SWT.RESIZE | SWT.TOOL;
				final int style = SWT.V_SCROLL | SWT.H_SCROLL;
				if (BrowserInformationControl.isAvailable(parent)) {
					return new BrowserInformationControl(parent, shellStyle,
							style);
				} else {
					return new DefaultInformationControl(parent, shellStyle,
							style, new HTMLTextPresenter(false));
				}
			}
		};
	}

	public IInformationControlCreator getHoverControlCreator() {
		return new IInformationControlCreator() {

			@SuppressWarnings("restriction")
			public IInformationControl createInformationControl(
					final Shell parent) {
				if (BrowserInformationControl.isAvailable(parent)) {
					return new BrowserInformationControl(parent, SWT.TOOL
							| SWT.NO_TRIM, SWT.NONE, EditorsUI
							.getTooltipAffordanceString());
				} else {
					return new DefaultInformationControl(parent, SWT.NONE,
							new HTMLTextPresenter(true), EditorsUI
									.getTooltipAffordanceString());
				}
			}
		};
	}

	public static String getHoverTextForOffset(final int offset,
			final ErlangEditor editor) {
		final ErlTextHover h = new ErlTextHover(
				ErlModelUtils.getModule(editor), editor.getExternalIncludes());
		final ITextViewer tv = editor.getViewer();
		final IRegion r = h.getHoverRegion(tv, offset);
		if (r == null) {
			return null;
		}
		return h.getHoverInfo(tv, r);
	}

}
