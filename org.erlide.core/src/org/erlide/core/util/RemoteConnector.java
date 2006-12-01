/*
 * Copyright 2001-2004 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.erlide.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.net.io.Util;

/*******************************************************************************
 * The reader thread reads from a local input source (presumably stdin) and
 * writes the data to a remote output destination. The writer thread reads from
 * a remote input source and writes to a local output destination. The threads
 * terminate when the remote input source closes.
 * 
 ******************************************************************************/

public class RemoteConnector {

	private Thread reader;

	private Thread writer;

	private OutputStream rOutput;

	public final void readWrite(final InputStream remoteInput,
			final OutputStream remoteOutput, final InputStream localInput,
			final OutputStream localOutput) {
		connectReadWrite(remoteInput, remoteOutput, localInput, localOutput);
		waitDisconnect();
	}

	public final void connectReadWrite(final InputStream remoteInput,
			final OutputStream remoteOutput, final InputStream localInput,
			final OutputStream localOutput) {
		rOutput = remoteOutput;

		reader = new Thread() {

			@Override
			public void run() {
				int ch;
				if (localInput == null) {
					return;
				}

				try {
					while (!interrupted() && (ch = localInput.read()) != -1) {
						remoteOutput.write(ch);
						remoteOutput.flush();
					}
				} catch (final IOException e) {
					// e.printStackTrace();
				}
			}
		};

		writer = new Thread() {

			@Override
			public void run() {
				try {
					if (localOutput == null) {
						return;
					}
					Util.copyStream(remoteInput, localOutput);
				} catch (final IOException e) {
					e.printStackTrace();
					// System.exit(1);
				}
			}
		};

		writer.setPriority(Thread.currentThread().getPriority() + 1);

		writer.start();
		reader.setDaemon(true);
		reader.start();

		try {
			Thread.sleep(100);
		} catch (final InterruptedException e) {
		}
	}

	public void waitDisconnect() {
		try {
			writer.join();
			reader.interrupt();
		} catch (final InterruptedException e) {
		}
	}

	public synchronized void write(byte[] bytes) {
		try {
			rOutput.write(bytes);
			rOutput.flush();
			Thread.yield();
		} catch (final IOException e1) {
			// TODO Auto-generated catch block
			// e1.printStackTrace();
		}
	}

	public void write(String str) {
		write(str.getBytes());
	}

}
