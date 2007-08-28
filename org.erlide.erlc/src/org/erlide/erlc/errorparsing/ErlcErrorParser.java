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
import org.erlide.core.IMarkerGenerator;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.erlc.core.ErrorParserManager;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlcErrorParser implements IErrorParser {

	public final static String ID = "erlcerrorparser";

	public boolean processLines(String lines, ErrorParserManager epm) {
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		OtpErlangObject res;
		try {
			res = b.rpcx("erlide_erlcerrors", "convert_erlc_errors",
					new OtpErlangString(lines));
			if (!(res instanceof OtpErlangList)) {
				return false;
			}

			addErrorMarkersForMultipleFiles(epm, (OtpErlangList) res);
		} catch (final ErlangRpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
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
	public static void addErrorMarkersForMultipleFiles(ErrorParserManager epm,
			OtpErlangList listOfFilesAndErrorLists) {
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
