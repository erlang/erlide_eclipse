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
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlModule;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.util.HTMLTextPresenter;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlTextHover implements ITextHover,
		IInformationProviderExtension2, ITextHoverExtension {

	// private ITextEditor fEditor;
	private OtpErlangList fImports;

	private IErlModule fModule;

	public ErlTextHover(IErlModule module) {
		fImports = null;
		fModule = module;
	}

	public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
		final ErlToken token = fModule.getScanner().getTokenAt(offset);
		if (token == null) {
			return null;
		}
		// ErlLogger.log("getHoverRegion " + token.toString());
		return new Region(token.getOffset(), token.getLength());
	}

	public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
		if (fImports == null) {
			fImports = ErlModelUtils.getImportsAsList(fModule);
		}
		OtpErlangObject r1 = null;
		try {

			final OtpErlangLong offset = new OtpErlangLong(hoverRegion
					.getOffset());

			final OtpErlangString s = new OtpErlangString(ErlideUIPlugin
					.getDefault().getStateLocation().toString());

			r1 = BackendUtil.checkRpc(BackendManager.getDefault()
					.getIdeBackend().rpc(
							"erlide_otp_doc",
							"get_doc_from_scan_tuples",
							new OtpErlangAtom("_erlide_"
									+ fModule.getElementName()), offset,
							fImports, s));

			if (r1 instanceof OtpErlangString) {
				final OtpErlangString s1 = (OtpErlangString) r1;
				return s1.stringValue();
			}
		} catch (final BackendException e) {
		}
		return null;
	}

	public IInformationControlCreator getInformationPresenterControlCreator() {
		return new IInformationControlCreator() {

			public IInformationControl createInformationControl(Shell parent) {
				final int shellStyle = SWT.RESIZE | SWT.TOOL;
				final int style = SWT.V_SCROLL | SWT.H_SCROLL;

				return new DefaultInformationControl(parent, shellStyle, style,
						new HTMLTextPresenter(false));
			}

		};
	}

	public IInformationControlCreator getHoverControlCreator() {
		return new IInformationControlCreator() {

			public IInformationControl createInformationControl(Shell parent) {
				return new DefaultInformationControl(parent, SWT.NONE,
						new HTMLTextPresenter(true), "Press 'F2' for focus");
			}
		};
	}

	public static String getHoverTextForOffset(int offset, ErlangEditor editor) {
		final ErlTextHover h = new ErlTextHover(ErlModelUtils.getModule(editor));
		final ITextViewer tv = editor.getViewer();
		final IRegion r = h.getHoverRegion(tv, offset);
		if (r == null) {
			return null;
		}
		return h.getHoverInfo(tv, r);
	}

}
