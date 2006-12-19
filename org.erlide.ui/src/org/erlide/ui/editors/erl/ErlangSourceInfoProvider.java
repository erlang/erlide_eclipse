/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.IInformationProviderExtension;

/**
 * ErlangSourceInfoProvider
 * 
 */
public class ErlangSourceInfoProvider implements IInformationProvider,
		IInformationProviderExtension {

	private ErlangEditor fEditor;

	/**
	 * 
	 */
	public ErlangSourceInfoProvider(ErlangEditor sourcePage) {
		fEditor = sourcePage;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.information.IInformationProvider#getInformation(org.eclipse.jface.text.ITextViewer,
	 *      org.eclipse.jface.text.IRegion)
	 */
	public String getInformation(ITextViewer textViewer, IRegion subject) {
		// This method is deprecated. Call the non-deprecated method
		return getInformation2(textViewer, subject).toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.information.IInformationProvider#getSubject(org.eclipse.jface.text.ITextViewer,
	 *      int)
	 */
	public IRegion getSubject(ITextViewer textViewer, int offset) {
		// Subject used in getInformation2
		if ((textViewer == null) || (fEditor == null)) {
			return null;
		}
		// Get the selected region
		IRegion region = ErlWordFinder.findWord(textViewer.getDocument(),
				offset);
		// Ensure the region is defined. Define an empty one if it is not.
		if (region == null) {
			return new Region(offset, 0);
		}
		return region;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.information.IInformationProviderExtension#getInformation2(org.eclipse.jface.text.ITextViewer,
	 *      org.eclipse.jface.text.IRegion)
	 */
	public Object getInformation2(ITextViewer textViewer, IRegion subject) {
		// Calls setInput on the quick outline popup dialog
		if ((textViewer == null) || (fEditor == null)) {
			return null;
		}
		// Get the current selection, if any
		Object selection = fEditor.getSelection();
		// If the input is null, then the dialog does not open
		// Define an empty object for no selection instead of null
		if (selection == null) {
			selection = new Object();
		}
		return selection;
	}

}
