package org.erlide.build;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.apache.tools.ant.BuildException;

public class ErlcTask extends org.apache.tools.ant.Task {

	private String erlRoot;

	private String action;

	private String prj;

	private String outputDir;

	private List<String> includeDirs;

	private List<String> sourceDirs;

	public void setErlRoot(String root) {
		this.erlRoot = root;
	}

	public String getErlRoot() {
		return erlRoot;
	}

	@Override
	public void execute() throws BuildException {
		if (erlRoot == null)
			erlRoot = getProject().getProperty("erlRoot");

		Properties p = new Properties();
		try {
			p.load(new FileInputStream(new File(prj.concat("/.codepath"))));
			outputDir = p.getProperty("output_dir");
			includeDirs = Arrays.asList(p.getProperty("include_dirs")
					.split(";"));
			sourceDirs = Arrays.asList(p.getProperty("source_dirs").split(";"));

			log(">> " + action + ": " + sourceDirs + " " + includeDirs + " "
					+ outputDir);

			if (action.equals("build")) {
				List<File> files;
				String cmd;

				boolean yrls = false;
				files = gatherFiles(sourceDirs, "yrl");
				for (File ff : files) {
					cmd = buildCommandLine(ff, outputDir);
					log("  " + cmd);
					exec(cmd);
					yrls = true;
				}

				if (yrls) {
					files = gatherFiles(Arrays
							.asList(new String[] { outputDir }), "erl");
					cmd = buildCommandLine(files, includeDirs, outputDir);
					log("  " + cmd);
					exec(cmd);
				}
				files = gatherFiles(sourceDirs, "erl");
				cmd = buildCommandLine(files, includeDirs, outputDir);
				log("  " + cmd);
				exec(cmd);
			} else if (action.equals("clean")) {

			} else {
				log("Unknown action " + action + ". Doing nothing.");
			}
		} catch (FileNotFoundException e) {
			throw new BuildException(e);
		} catch (IOException e) {
			throw new BuildException(e);
		}
	}

	private void exec(String cmd) {
		try {
			Process r = Runtime.getRuntime().exec(cmd);
			InputStream i = r.getInputStream();
			OutputStreamMonitor m = new OutputStreamMonitor(i);
			m.startMonitoring();
			InputStream ie = r.getErrorStream();
			OutputStreamMonitor me = new OutputStreamMonitor(ie);
			me.startMonitoring();
			while (true) {
				try {
					Thread.sleep(200);
					// int x = r.exitValue();
					break;
				} catch (IllegalThreadStateException ex) {
				} catch (InterruptedException e) {
				}
			}
			m.kill();
			me.kill();
		} catch (IOException e) {
			log("*** " + e.getMessage());
		}
	}

	private List<File> gatherFiles(List<String> sources, final String ext) {
		List<File> result = new ArrayList<File>(10);

		for (String d : sources) {
			File dir = new File(prj + "/" + d);
			FilenameFilter filter = new FilenameFilter() {
				public boolean accept(File aDir, String name) {
					return name.endsWith(ext);
				}
			};
			List<File> ff = Arrays.asList(dir.listFiles(filter));
			result.addAll(ff);
		}
		return result;
	}

	private String buildCommandLine(List<File> files, List<String> includes,
			String output) {
		String result = erlRoot.concat("/bin/erlc").concat(" -o ").concat(
				prj.concat("/" + output));
		for (String s : includes)
			result = result + " -I " + prj + "/" + s;
		for (File f : files)
			result = result + " " + f.getAbsolutePath();
		return result;
	}

	private String buildCommandLine(File file, String output) {
		String result = erlRoot.concat("/bin/erlc ").concat("-o ").concat(
				prj.concat("/" + output));
		result = result + " " + file.getAbsolutePath();
		return result;
	}

	// private String buildCommandLine(File file, List<String> includes,
	// String output) {
	// ArrayList<File> list = new ArrayList<File>(1);
	// list.add(file);
	// return buildCommandLine(list, includes, output);
	// }

	public String getPrj() {
		return this.prj;
	}

	public void setPrj(String project) {
		this.prj = project;
	}

	public class OutputStreamMonitor {

		/**
		 * The stream being monitored (connected system out or err).
		 */
		private InputStream fStream;

		/**
		 * Whether content is being buffered
		 */
		private boolean fBuffered = true;

		/**
		 * The local copy of the stream contents
		 */
		private StringBuffer fContents;

		/**
		 * The thread which reads from the stream
		 */
		private Thread fThread;

		/**
		 * The size of the read buffer
		 */
		private static final int BUFFER_SIZE = 8192;

		/**
		 * Whether or not this monitor has been killed. When the monitor is
		 * killed, it stops reading from the stream immediately.
		 */
		private boolean fKilled = false;

		private long lastSleep;

		/**
		 * Creates an output stream monitor on the given stream (connected to
		 * system out or err).
		 */
		public OutputStreamMonitor(InputStream stream) {
			fStream = new BufferedInputStream(stream, 8192);
			fContents = new StringBuffer();
		}

		/**
		 * Causes the monitor to close all communications between it and the
		 * underlying stream by waiting for the thread to terminate.
		 */
		protected void close() {
			if (fThread != null) {
				final Thread thread = fThread;
				fThread = null;
				try {
					thread.join();
				} catch (final InterruptedException ie) {
				}
			}
		}

		/**
		 * Notifies the listeners that text has been appended to the stream.
		 */
		private void fireStreamAppended(String text) {
			log(text);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.debug.core.model.IStreamMonitor#getContents()
		 */
		public synchronized String getContents() {
			return fContents.toString();
		}

		/**
		 * Continually reads from the stream.
		 * <p>
		 * This method, along with the <code>startReading</code> method is
		 * used to allow <code>OutputStreamMonitor</code> to implement
		 * <code>Runnable</code> without publicly exposing a <code>run</code>
		 * method.
		 */
		protected void read() {
			lastSleep = System.currentTimeMillis();
			long currentTime = lastSleep;
			final byte[] bytes = new byte[BUFFER_SIZE];
			int read = 0;
			while (read >= 0) {
				try {
					if (fKilled) {
						break;
					}
					read = fStream.read(bytes);
					if (read < 0) {
						return;
					}
					if (read > 0) {
						final String text = new String(bytes, 0, read);
						synchronized (this) {
							if (isBuffered()) {
								fContents.append(text);
							}
							fireStreamAppended(text);
						}
					}
				} catch (final SocketException se) {
					return;
				} catch (final IOException ioe) {
					return;
				} catch (final NullPointerException e) {
					// killing the stream monitor while reading can cause an NPE
					// when reading from the stream
					if (!fKilled && fThread != null) {
						ErlcTask.this.log(e.toString());
					}
					return;
				}

				currentTime = System.currentTimeMillis();
				if (currentTime - lastSleep > 1000) {
					lastSleep = currentTime;
					try {
						Thread.sleep(1); // just give up CPU to maintain UI
						// responsiveness.
					} catch (final InterruptedException e) {
					}
				}
			}
			try {
				fStream.close();
			} catch (final IOException e) {
				ErlcTask.this.log(e.toString());
			}
		}

		protected void kill() {
			fKilled = true;
		}

		/**
		 * Starts a thread which reads from the stream
		 */
		public void startMonitoring() {
			if (fThread == null) {
				fThread = new Thread(new Runnable() {

					public void run() {
						read();
					}
				}, "OutputStreamMonitor"); //$NON-NLS-1$
				fThread.setDaemon(true);
				fThread.setPriority(Thread.MIN_PRIORITY);
				fThread.start();
			}
		}

		/**
		 * @see org.eclipse.debug.core.model.IFlushableStreamMonitor#setBuffered(boolean)
		 */
		public synchronized void setBuffered(boolean buffer) {
			fBuffered = buffer;
		}

		/**
		 * @see org.eclipse.debug.core.model.IFlushableStreamMonitor#flushContents()
		 */
		public synchronized void flushContents() {
			fContents.setLength(0);
		}

		/**
		 * @see IFlushableStreamMonitor#isBuffered()
		 */
		public synchronized boolean isBuffered() {
			return fBuffered;
		}

	}

	public String getAction() {
		return this.action;
	}

	public void setAction(String action) {
		this.action = action;
	}

}