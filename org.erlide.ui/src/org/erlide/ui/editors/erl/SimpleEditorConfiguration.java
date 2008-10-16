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
package org.erlide.ui.editors.erl;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.ui.util.IColorManager;

public class SimpleEditorConfiguration extends EditorConfiguration {

	public SimpleEditorConfiguration(IPreferenceStore store,
			ErlangEditor leditor, IColorManager lcolorManager) {
		super(store, leditor, lcolorManager);
	}

	/**
	 * The double click strategy
	 * 
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getDoubleClickStrategy(org.eclipse.jface.text.source.ISourceViewer,
	 *      java.lang.String)
	 */
	@Override
	public ITextDoubleClickStrategy getDoubleClickStrategy(
			ISourceViewer sourceViewer, String contentType) {
		return null;
	}

	/**
	 * 
	 * @param sourceViewer
	 * @param contentType
	 * @return
	 */
	public IAutoEditStrategy getAutoEditStrategy(ISourceViewer sourceViewer,
			String contentType) {
		return null;
	}

	@Override
	public ITextHover getTextHover(ISourceViewer sourceViewer,
			String contentType) {
		return null;
	}

}
