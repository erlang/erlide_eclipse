/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlModelUtils {

	public static IErlModule getModule(ITextEditor editor) {
		if (editor == null) {
			return null;
		}
		return getModule(editor.getEditorInput());
	}

	public static IErlModule getModule(IEditorInput editorInput) {

		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			return getModule(input.getFile());
		}
		return null;
	}

	public static IErlModule getModule(IFile file) {
		final String prj = file.getProject().getName();
		final IErlModel mdl = ErlangCore.getModel();

		try {
			mdl.open(null);
			return mdl.getErlangProject(prj).getModule(file.getName());
		} catch (final ErlModelException e) {
			return null;
		}
	}

	public static IErlProject getErlProject(ITextEditor editor) {
		return getErlProject(editor.getEditorInput());
	}

	public static IErlProject getErlProject(IEditorInput editorInput) {
		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) editorInput;
			final IErlModel mdl = ErlangCore.getModel();

			final String prj = input.getFile().getProject().getName();

			try {
				mdl.open(null);
				return mdl.getErlangProject(prj);
			} catch (final ErlModelException e) {
				return null;
			}
		}
		return null;
	}

	public static IErlFunction findFunction(IErlModule module, String function,
			int arity) throws ErlModelException {
		final IErlElement[] children = module.getChildren();
		for (IErlElement element : children) {
			if (element instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) element;
				if (arity == -1 || f.getArity() == arity) {
					if (f.getElementName().equals(function)) {
						return f;
					}
				}
			}
		}
		return null;
	}

	public static OtpErlangList getImportsAsList(IErlModule mod) {
		if (mod == null) {
			return new OtpErlangList();
		}

		final IErlImport[] imports = mod.getImports();
		final OtpErlangTuple rImports[] = new OtpErlangTuple[imports.length];
		for (int i = 0; i < imports.length; ++i) {
			final IErlImport imp = imports[i];
			final ErlangFunction[] impFuncs = imp.getFunctions();
			final OtpErlangTuple rImpFuncs[] = new OtpErlangTuple[impFuncs.length];
			for (final ErlangFunction f : impFuncs) {
				rImpFuncs[i] = new OtpErlangTuple(new OtpErlangAtom(f.name),
						new OtpErlangLong(f.arity));
			}
			rImports[i] = new OtpErlangTuple(new OtpErlangAtom(imp
					.getImportModule()), new OtpErlangList(rImpFuncs));
		}
		return new OtpErlangList(rImports);
	}

	public static IErlScanner getScanner(ITextEditor editor) {
		final IErlModule mod = getModule(editor);
		ErlLogger.debug("getScanner:: " + editor + " = " + mod);
		if (mod != null) {
			return getModule(editor).getScanner();
		}
		return null;
	}

}
