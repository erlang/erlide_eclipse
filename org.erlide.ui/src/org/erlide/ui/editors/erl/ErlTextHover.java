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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
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
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.util.HTMLTextPresenter;
import org.erlide.ui.util.ErlModelUtils;

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

	public ErlTextHover(final IErlModule module) {
		fImports = null;
		fModule = module;
	}

	public IRegion getHoverRegion(final ITextViewer textViewer, final int offset) {
		final ErlToken token = fModule.getScanner().getTokenAt(offset);
		if (token == null) {
			return null;
		}
		// ErlLogger.debug("getHoverRegion " + token.toString());
		return new Region(token.getOffset(), token.getLength());
	}

	public String getHoverInfo(final ITextViewer textViewer,
			final IRegion hoverRegion) {
		if (fImports == null) {
			fImports = ErlModelUtils.getImportsAsList(fModule);
		}
		OtpErlangObject r1 = null;
		final int offset = hoverRegion.getOffset();
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		final BuildBackend b = BackendManager.getDefault().getIdeBackend()
				.asBuild();
		r1 = ErlideDoc.getDocFromScan(b, offset, stateDir, ErlScanner
				.createScannerModuleName(fModule), fImports);
		if (r1 instanceof OtpErlangString) {
			final OtpErlangString s1 = (OtpErlangString) r1;
			return s1.stringValue();
		} else if (r1 instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) r1;
			final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(1);
			String definedName = a.atomValue();
			if (definedName.charAt(0) == '?') {
				definedName = definedName.substring(1);
			}
			try {
				final IErlPreprocessorDef pd = ErlModelUtils
						.findPreprocessorDef((IProject) fModule.getProject()
								.getResource(), fModule, definedName,
								IErlElement.Kind.MACRO_DEF,
								new ArrayList<IErlModule>());
				return pd.getExtra();
			} catch (final CoreException e) {
				e.printStackTrace();
			}
		}
		return null;
	}

	public IInformationControlCreator getInformationPresenterControlCreator() {
		return new IInformationControlCreator() {

			public IInformationControl createInformationControl(
					final Shell parent) {
				final int shellStyle = SWT.RESIZE | SWT.TOOL;
				final int style = SWT.V_SCROLL | SWT.H_SCROLL;

				return new DefaultInformationControl(parent, shellStyle, style,
						new HTMLTextPresenter(false));
			}

		};
	}

	public IInformationControlCreator getHoverControlCreator() {
		return new IInformationControlCreator() {

			public IInformationControl createInformationControl(
					final Shell parent) {
				return new DefaultInformationControl(parent, SWT.NONE,
						new HTMLTextPresenter(true), "Press 'F2' for focus");
			}
		};
	}

	public static String getHoverTextForOffset(final int offset,
			final ErlangEditor editor) {
		final ErlTextHover h = new ErlTextHover(ErlModelUtils.getModule(editor));
		final ITextViewer tv = editor.getViewer();
		final IRegion r = h.getHoverRegion(tv, offset);
		if (r == null) {
			return null;
		}
		return h.getHoverInfo(tv, r);
	}

}
