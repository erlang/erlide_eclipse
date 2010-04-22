package org.erlide.jinterface.util;

import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;


public class ProcessRunner {
	private static final Logger log = Logger.getLogger(ProcessRunner.class.getName());
	private ProcessBuilder pb;
	private String message;

	public ProcessRunner(String command, String bpel) {
		pb = new ProcessBuilder(command, bpel);
	}

	public boolean run(boolean synchronous) {
		Process p = null;
		try {
			p = pb.start();
			InputStreamReaderThread errorReader = new InputStreamReaderThread(p
					.getErrorStream());
			errorReader.start();
			InputStreamReaderThread inputReader = new InputStreamReaderThread(p
					.getInputStream());
			inputReader.start();
			p.waitFor();
			if (errorReader.getInput() != null) {
				message = errorReader.getInput();
				return false;
			}
		} catch (Throwable t) {
			log.log(Level.SEVERE, "Could not validate process", t);
			message = "Error occurred while validating process";
			return false;
		}
		return true;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	private class InputStreamReaderThread extends Thread {
		private InputStream is;
		private String input;

		public InputStreamReaderThread(InputStream is) {
			this.is = is;
		}

		@Override
		public void run() {
			try {
				//FIXME IOUtils is in some apache.commons libs
				byte[] output = null;// = IOUtils.toByteArray(is);
				input = new String(output);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		public String getInput() {
			return input;
		}
	}

}
