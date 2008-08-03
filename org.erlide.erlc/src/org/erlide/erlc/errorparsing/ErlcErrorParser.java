/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.errorparsing;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.erlide.core.IMarkerGenerator;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.erlc.core.ErrorParserManager;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BuildBackend;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideBackend;

public class ErlcErrorParser implements IErrorParser {

	public final static String ID = "erlcerrorparser";

	public boolean processLines(final String lines,
			final ErrorParserManager epm, final IProject project) {
		final BuildBackend b = BackendManager.getDefault().getBuild(project);
		OtpErlangObject res;
		try {
			res = ErlideBackend.convertErrors(b, lines);
			if (!(res instanceof OtpErlangList)) {
				return false;
			}
			addErrorMarkersForMultipleFiles(epm, (OtpErlangList) res);
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	/**
	 * Add from a list of files and errors
	 * 
	 * @param mg
	 * @param listOfFilesAndErrorLists
	 */
	public static void addErrorMarkersForMultipleFiles(
			final ErrorParserManager epm,
			final OtpErlangList listOfFilesAndErrorLists) {
		final IMarkerGenerator mg = epm.getMarkerGenerator();
		for (int i = 0; i < listOfFilesAndErrorLists.arity(); ++i) {
			final OtpErlangObject o = listOfFilesAndErrorLists.elementAt(i);
			final OtpErlangTuple t = (OtpErlangTuple) o;
			final OtpErlangString fileName = (OtpErlangString) t.elementAt(0);
			final OtpErlangList l = (OtpErlangList) t.elementAt(1);
			final IFile f = epm.findFileName(fileName.stringValue());
			ErlangBuilder.addErrorMarkers(mg, f, l);
		}
	}

}
