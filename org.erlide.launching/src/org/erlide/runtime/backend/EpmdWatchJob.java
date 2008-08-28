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
package org.erlide.runtime.backend;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.ericsson.otp.erlang.OtpEpmd;

/**
 * Each 2 seconds, query epmd to see if there are any new nodes that have been
 * registered.
 * 
 */
public class EpmdWatchJob extends Job {

	// TODO add a way to register several hosts to watch

	// TODO maybe better to register node names we're interested in, to be
	// notified when they go up/down?

	public EpmdWatchJob() {
		super("Checking EPMD for new backends");
		setSystem(true);
		setPriority(SHORT);
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {

		checkEpmd();

		this.schedule(1000);
		return Status.OK_STATUS;
	}

	private List<String> nodes = new ArrayList<String>();

	private void checkEpmd() {

		try {
			final String[] names = OtpEpmd.lookupNames();
			final List<String> labels = Arrays.asList(names);

			List<String> started = getDiff(labels, nodes);
			List<String> stopped = getDiff(nodes, labels);

			clean(started);
			clean(stopped);

			BackendManager.getDefault().updateBackendStatus(started, stopped);

			nodes = labels;

		} catch (final IOException e) {
			e.printStackTrace();
		}

	}

	public static void clean(List<String> list) {
		for (int i = 0; i < list.size(); i++) {
			String label = list.get(i);
			// label is "name X at port N"
			final String[] parts = label.split(" ");
			if (parts.length == 5) {
				String alabel = parts[1];
				if (alabel.length() == 0) {
					alabel = "??" + label;
				}
				list.set(i, alabel);
			}
		}
	}

	private List<String> getDiff(List<String> list1, List<String> list2) {
		List<String> result = new ArrayList<String>(list1);
		result.removeAll(list2);
		return result;
	}

}
